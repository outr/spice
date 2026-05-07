package spice.http.server.acme

import rapid.Task
import scribe.Priority
import scribe.mdc.MDC
import spice.http.HttpExchange
import spice.http.server.{HttpServer, MutableHttpServer}
import spice.http.server.config.{HttpServerListener, HttpsServerListener, KeyStore => SpiceKeyStore}
import spice.http.server.dsl.{ConnectionFilter, FilterResponse}
import spice.http.server.handler.HttpHandler

import scala.concurrent.duration.*

/** High-level coordinator for ACME-managed HTTPS.
  *
  * Typical wiring with a [[MutableHttpServer]]:
  * {{{
  *   val config = AcmeConfig(
  *     contactEmail = "ops@example.com",
  *     domains      = List("example.com"),
  *     storageDir   = new File("data/acme"),
  *     staging      = false
  *   )
  *   val server = new MutableHttpServer
  *
  *   val acme = AcmeManager.install(server, config)
  *   for {
  *     _ <- acme.ensureCert            // issues if missing/expiring; loads otherwise
  *     _ <- server.start()             // HTTP+HTTPS listeners are now live
  *     _ <- acme.renewalLoop(server.restart()).start()  // background renewer
  *   } yield ()
  * }}}
  *
  * `install` installs the challenge filter and configures the HTTP+HTTPS listeners. The
  * HTTP listener is required for HTTP-01 validation; the HTTPS listener reads the JKS
  * keystore that `ensureCert` writes.
  *
  * The renewal loop restarts the server after a successful re-issue so the new cert takes
  * effect. This drops in-flight TLS connections — that's the documented MVP tradeoff.
  * Zero-downtime cert swap (Undertow rebuild without listener teardown) is future work. */
class AcmeManager private (val config: AcmeConfig,
                           val challengeStore: AcmeChallengeStore) {
  /** True if the keystore exists and its leaf cert is not within `config.renewBefore` of expiry. */
  def hasValidCert: Boolean =
    AcmeKeyStore.certNotAfter(config.keystoreFile, config.keystorePassword, config.primaryDomain)
      .exists(notAfter => !AcmeKeyStore.needsRenewal(notAfter, config.renewBefore))

  /** Idempotent: returns immediately if [[hasValidCert]] is true; otherwise drives the
    * full ACME order against the configured directory and writes a fresh JKS. The caller
    * is responsible for ensuring the challenge filter is reachable on port 80 from the CA
    * before invoking this — `install` does that automatically.
    *
    * Errors propagate as a failed `Task`. The cert flow is synchronous inside the Task
    * but doesn't block the calling thread (rapid runs it on its own pool). */
  def ensureCert: Task[Unit] = Task {
    if (!hasValidCert) {
      AcmeClient.provision(config, challengeStore).sync()
    }
  }

  /** Forever: sleep `config.renewCheckInterval`, check expiry, re-issue if needed.
    * After a successful re-issue, calls `server.reloadCertificates()` to atomically
    * swap the new cert into the running HTTPS listeners — no restart, no dropped
    * connections. Loop exits only via cancellation. */
  def renewalLoop(server: HttpServer): Task[Unit] = {
    def step: Task[Unit] = Task.sleep(config.renewCheckInterval).flatMap { _ =>
      val expired = AcmeKeyStore.certNotAfter(config.keystoreFile, config.keystorePassword, config.primaryDomain) match {
        case Some(notAfter) => AcmeKeyStore.needsRenewal(notAfter, config.renewBefore)
        case None           => true // no keystore → treat as needing renewal
      }
      if (expired) {
        AcmeClient.provision(config, challengeStore)
          .flatMap(_ => server.reloadCertificates())
          .handleError { t =>
            scribe.error("ACME renewal failed; will retry on next interval", t)
            Task.unit
          }
      } else Task.unit
    }.flatMap(_ => step)
    step
  }
}

object AcmeManager {
  /** Wire ACME into a [[MutableHttpServer]].
    *
    * For HTTP-01 (default):
    *   1. Install the challenge filter as a `Priority.Highest` handler.
    *   2. Add an HTTP listener on `httpPort` so the CA can reach `.well-known/acme-challenge`.
    *   3. Add an HTTPS listener on `httpsPort` pointing at the managed JKS.
    *
    * For DNS-01:
    *   - The challenge filter and HTTP listener are NOT added (DNS validation doesn't
    *     touch the server). If you want HTTP for redirects or other reasons, configure
    *     it yourself.
    *   - HTTPS listener is added as above.
    *
    * Listeners are added to the server's existing list — your other listeners survive.
    * This does NOT issue a cert; call `manager.ensureCert` separately. */
  def install(server: MutableHttpServer,
              config: AcmeConfig,
              httpPort: Int = 80,
              httpsPort: Int = 443,
              bindHost: String = "0.0.0.0"): AcmeManager = {
    val store = new AcmeChallengeStore

    config.challenge match {
      case AcmeChallenge.Http01 =>
        server.handlers += new ChallengeHandler(AcmeChallengeFilter(store))
        server.config.addListeners(
          HttpServerListener(host = bindHost, port = Some(httpPort)),
          httpsListener(config, bindHost, httpsPort)
        )
      case _: AcmeChallenge.Dns01 =>
        server.config.addListeners(httpsListener(config, bindHost, httpsPort))
    }

    new AcmeManager(config, store)
  }

  private def httpsListener(config: AcmeConfig, bindHost: String, httpsPort: Int): HttpsServerListener =
    HttpsServerListener(
      host     = bindHost,
      port     = Some(httpsPort),
      keyStore = SpiceKeyStore(path = config.keystoreFile.getPath, password = config.keystorePassword),
      enabled  = true
    )

  /** Construct a manager without modifying a server. Use this when you want to drive the
    * cert flow against a custom server setup (e.g. you've already added your own HTTP
    * listener and just want to add the challenge filter manually).
    *
    * The caller is responsible for installing the returned `challengeFilter` somewhere
    * the CA's HTTP-01 probe will reach it. */
  def manual(config: AcmeConfig): AcmeManager = new AcmeManager(config, new AcmeChallengeStore)

  /** Wraps an `AcmeChallengeFilter` as a top-priority `HttpHandler` so it runs before
    * any user-installed handler. The filter itself decides whether to claim the request
    * (matching path) or pass through. */
  private final class ChallengeHandler(filter: AcmeChallengeFilter) extends HttpHandler {
    override def priority: Priority = Priority.Highest
    override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = filter.handle(exchange)
  }
}
