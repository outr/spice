package spice.http.server.acme

import org.shredzone.acme4j.challenge.{Dns01Challenge, Http01Challenge}
import org.shredzone.acme4j.{Account, AccountBuilder, Authorization, Order, Session, Status}
import rapid.Task

import java.security.cert.X509Certificate
import java.time.Duration as JDuration
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Drives a single ACME order: account registration, challenge dance (HTTP-01 or DNS-01),
  * finalization, and certificate download.
  *
  * Polling waits use acme4j's built-in `waitForCompletion(Duration)` which respects the
  * `Retry-After` header the CA returns — much friendlier to the upstream than a fixed
  * sleep loop. Each step has its own bounded budget so a stuck challenge fails fast
  * rather than hanging server start. */
object AcmeClient {
  private val AuthBudget: FiniteDuration  = 5.minutes  // DNS-01 typically slower than HTTP-01
  private val OrderBudget: FiniteDuration = 1.minute
  private val CertBudget: FiniteDuration  = 1.minute

  /** Issue a fresh certificate for [[AcmeConfig.domains]].
    *
    * Side effects, in order:
    *   1. `account.key` and `domain.key` are created on disk if missing (PEM).
    *   2. ACME account is registered with the CA (no-op if the key was already registered).
    *   3. Challenges are presented per `config.challenge`:
    *      - HTTP-01: token published into `challengeStore` (for the challenge filter).
    *      - DNS-01: TXT records published via the configured
    *        [[spice.http.server.acme.dns.DnsProvider]].
    *   4. On success, [[AcmeConfig.keystoreFile]] is overwritten with the new key + chain.
    *   5. Cleanup runs unconditionally in `finally` (remove tokens / unpublish records).
    *
    * `challengeStore` is consulted only for HTTP-01; for DNS-01-only configs an empty
    * store is fine (it's never read). */
  def provision(config: AcmeConfig, challengeStore: AcmeChallengeStore): Task[List[X509Certificate]] = {
    config.challenge match {
      case AcmeChallenge.Http01     => provisionHttp01(config, challengeStore)
      case dns: AcmeChallenge.Dns01 => provisionDns01(config, dns)
    }
  }

  private def provisionHttp01(config: AcmeConfig, challengeStore: AcmeChallengeStore): Task[List[X509Certificate]] = Task {
    val (order, domainKey) = beginOrder(config)
    try {
      order.getAuthorizations.asScala.foreach(auth => completeHttp01(auth, challengeStore))
      finalizeAndStore(config, order, domainKey)
    } finally {
      // Drain published tokens so a failed run doesn't leak entries into the store.
      order.getAuthorizations.asScala.foreach { auth =>
        val opt = auth.findChallenge(classOf[Http01Challenge])
        if (opt.isPresent) challengeStore.remove(opt.get().getToken)
      }
    }
  }

  private def provisionDns01(config: AcmeConfig, dns: AcmeChallenge.Dns01): Task[List[X509Certificate]] = Task {
    val (order, domainKey) = beginOrder(config)
    val auths = order.getAuthorizations.asScala.toList
    // Track every (recordName, value) we publish so the cleanup pass can find them.
    val published = scala.collection.mutable.ListBuffer.empty[(String, String)]
    try {
      auths.foreach { auth =>
        if (auth.getStatus != Status.VALID) {
          val opt = auth.findChallenge(classOf[Dns01Challenge])
          if (!opt.isPresent) {
            throw new IllegalStateException(
              s"Authorization for ${auth.getIdentifier.getDomain} does not offer a DNS-01 challenge"
            )
          }
          val challenge = opt.get()
          val recordName = Dns01Challenge.toRRName(auth.getIdentifier)
          val value      = challenge.getDigest
          dns.provider.publish(recordName, value).sync()
          published += (recordName -> value)
          if (dns.propagationWait.toMillis > 0) Thread.sleep(dns.propagationWait.toMillis)
          challenge.trigger()
          val finalStatus = auth.waitForCompletion(JDuration.ofMillis(AuthBudget.toMillis))
          if (finalStatus != Status.VALID) {
            val errorDetail = {
              val e = challenge.getError
              if (e.isPresent) e.get().toString else "no error detail provided"
            }
            throw new RuntimeException(
              s"ACME DNS-01 authorization for ${auth.getIdentifier.getDomain} did not become VALID (status=$finalStatus): $errorDetail"
            )
          }
        }
      }
      finalizeAndStore(config, order, domainKey)
    } finally {
      published.foreach { case (name, value) =>
        try dns.provider.unpublish(name, value).sync()
        catch { case t: Throwable => scribe.warn(s"Failed to unpublish DNS record $name: ${t.getMessage}") }
      }
    }
  }

  /** Common setup: load/create both keys, open a session, register the account, place the
    * order. Returns everything the per-challenge driver needs. */
  private def beginOrder(config: AcmeConfig): (Order, java.security.KeyPair) = {
    config.storageDir.mkdirs()
    val accountKey = AcmeKeyStore.loadOrCreateKeyPair(config.accountKeyFile)
    val domainKey  = AcmeKeyStore.loadOrCreateKeyPair(config.domainKeyFile)
    val session = new Session(config.resolvedDirectoryUrl)
    val account = registerAccount(session, accountKey, config.contactEmail)
    val order   = account.newOrder().domains(config.domains.asJava).create()
    (order, domainKey)
  }

  /** Common finalize+download+store path, shared by both challenge types. */
  private def finalizeAndStore(config: AcmeConfig, order: Order, domainKey: java.security.KeyPair): List[X509Certificate] = {
    finalizeOrder(order, domainKey)
    val chain = waitForCertificate(order)
    AcmeKeyStore.writeKeystore(
      file       = config.keystoreFile,
      password   = config.keystorePassword,
      alias      = config.primaryDomain,
      domainKey  = domainKey,
      chain      = chain.toArray
    )
    chain
  }

  private def registerAccount(session: Session, accountKey: java.security.KeyPair, email: String): Account = {
    new AccountBuilder()
      .addEmail(email)
      .agreeToTermsOfService()
      .useKeyPair(accountKey)
      .create(session)
  }

  /** Publish the HTTP-01 token into the challenge store, trigger validation, wait for the
    * authorization to settle. acme4j's `waitForCompletion` polls with `Retry-After`
    * backoff. */
  private def completeHttp01(auth: Authorization, store: AcmeChallengeStore): Unit = {
    if (auth.getStatus == Status.VALID) return
    val opt = auth.findChallenge(classOf[Http01Challenge])
    if (!opt.isPresent) {
      throw new IllegalStateException(
        s"Authorization for ${auth.getIdentifier.getDomain} does not offer an HTTP-01 challenge"
      )
    }
    val challenge = opt.get()
    store.put(challenge.getToken, challenge.getAuthorization)
    challenge.trigger()

    val finalStatus = auth.waitForCompletion(JDuration.ofMillis(AuthBudget.toMillis))
    if (finalStatus != Status.VALID) {
      val errorDetail = {
        val e = challenge.getError
        if (e.isPresent) e.get().toString else "no error detail provided"
      }
      throw new RuntimeException(
        s"ACME HTTP-01 authorization for ${auth.getIdentifier.getDomain} did not become VALID (status=$finalStatus): $errorDetail"
      )
    }
  }

  /** Submit the CSR (acme4j builds it from the domain key + order identifiers) and wait
    * for the order to be VALID — at which point the certificate URL is available. */
  private def finalizeOrder(order: Order, domainKey: java.security.KeyPair): Unit = {
    order.execute(domainKey)
    val finalStatus = order.waitForCompletion(JDuration.ofMillis(OrderBudget.toMillis))
    if (finalStatus != Status.VALID) {
      val errorDetail = {
        val e = order.getError
        if (e.isPresent) e.get().toString else "no error detail provided"
      }
      throw new RuntimeException(s"ACME order finalize did not become VALID (status=$finalStatus): $errorDetail")
    }
  }

  /** After the order is VALID the cert URL is set; download and return the chain. */
  private def waitForCertificate(order: Order): List[X509Certificate] = {
    val deadline = System.currentTimeMillis() + CertBudget.toMillis
    while (System.currentTimeMillis() < deadline) {
      val cert = order.getCertificate
      if (cert != null) {
        return cert.getCertificateChain.asScala.toList
      }
      Thread.sleep(2000)
      order.fetch()
    }
    throw new RuntimeException(s"ACME certificate did not arrive within $CertBudget")
  }
}
