package spice.http.server.acme

import java.io.File
import scala.concurrent.duration.*

/** Configuration for automatic HTTPS certificate provisioning via ACME (Let's Encrypt et al.).
  *
  * The validation method depends on `challenge`:
  *   - [[AcmeChallenge.Http01]] (default): CA hits `http://<domain>/.well-known/acme-challenge/<token>`.
  *     Server must be reachable from the public internet on port 80. Doesn't support
  *     wildcards — `*.example.com` requires DNS-01.
  *   - [[AcmeChallenge.Dns01]]: CA queries `_acme-challenge.<domain>` TXT. Requires a
  *     [[spice.http.server.acme.dns.DnsProvider]] to publish records. Required for
  *     wildcards.
  *
  * @param contactEmail   Account-recovery email address registered with the CA. Required by
  *                       Let's Encrypt and most other CAs.
  * @param domains        Identifiers to issue the certificate for. The first entry is the CN;
  *                       the rest become Subject Alternative Names. Wildcards (`*.example.com`)
  *                       are allowed only with `challenge = AcmeChallenge.Dns01(...)`.
  * @param storageDir     Directory where the account key, domain key, and JKS keystore are
  *                       persisted between runs. Created if missing.
  * @param keystorePassword
  *                       Password for the generated JKS keystore. The default is sufficient
  *                       for most deployments — the keystore lives on the same disk as the
  *                       running server, so the password isn't a security boundary. Override
  *                       only if a downstream tool requires a specific value.
  * @param challenge      Validation method. Defaults to HTTP-01.
  * @param staging        When true, point at the Let's Encrypt staging directory instead of
  *                       production. Use for development — staging certs are signed by an
  *                       untrusted root, but they don't count against the production rate
  *                       limit (5 cert orders / week / domain).
  * @param renewBefore    Begin renewal this far before the current cert's `notAfter`.
  *                       Default 30 days matches Let's Encrypt's recommendation. The renewal
  *                       check itself runs every `renewCheckInterval`.
  * @param renewCheckInterval
  *                       How often to wake up and check whether `notAfter - now < renewBefore`.
  *                       Default 12 hours. The check is cheap (just inspects an X509 cert in
  *                       memory); the actual renewal work only runs when the threshold trips.
  * @param directoryUrl   Override the ACME directory URL. When set, takes precedence over
  *                       `staging`. Use to point at a non-Let's-Encrypt CA (ZeroSSL, BuyPass)
  *                       or a local pebble/boulder for testing.
  */
case class AcmeConfig(contactEmail: String,
                      domains: List[String],
                      storageDir: File,
                      keystorePassword: String = "spice-acme",
                      challenge: AcmeChallenge = AcmeChallenge.Http01,
                      staging: Boolean = false,
                      renewBefore: FiniteDuration = 30.days,
                      renewCheckInterval: FiniteDuration = 12.hours,
                      directoryUrl: Option[String] = None) {
  require(domains.nonEmpty, "AcmeConfig.domains must contain at least one domain")
  require(contactEmail.contains('@'), s"AcmeConfig.contactEmail is not an email address: $contactEmail")
  require(
    challenge != AcmeChallenge.Http01 || !domains.exists(_.startsWith("*.")),
    s"Wildcard domains (${domains.filter(_.startsWith("*.")).mkString(", ")}) require AcmeChallenge.Dns01 — HTTP-01 cannot validate wildcards."
  )

  /** Resolved ACME directory URL. `directoryUrl` overrides `staging`; otherwise picks
    * Let's Encrypt staging or production based on `staging`. */
  lazy val resolvedDirectoryUrl: String = directoryUrl.getOrElse {
    if (staging) AcmeConfig.LetsEncryptStaging else AcmeConfig.LetsEncryptProduction
  }

  lazy val accountKeyFile: File = new File(storageDir, "account.key")
  lazy val domainKeyFile: File = new File(storageDir, "domain.key")
  lazy val keystoreFile: File = new File(storageDir, "keystore.jks")

  /** The CN (first entry of `domains`) — used as the keystore alias. */
  lazy val primaryDomain: String = domains.head
}

object AcmeConfig {
  val LetsEncryptProduction: String = "acme://letsencrypt.org"
  val LetsEncryptStaging: String = "acme://letsencrypt.org/staging"
}
