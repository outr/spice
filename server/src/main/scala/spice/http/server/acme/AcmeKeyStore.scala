package spice.http.server.acme

import org.shredzone.acme4j.util.KeyPairUtils

import java.io.{File, FileReader, FileWriter}
import java.nio.file.Files
import java.security.cert.X509Certificate
import java.security.{KeyPair, KeyStore, SecureRandom}
import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** Disk-side persistence for ACME-managed keys and certificates.
  *
  * Three files live under [[AcmeConfig.storageDir]]:
  *   - `account.key` — the account-level key pair registered with the CA. Identifies the
  *     ACME account across renewals; rotating it forces a re-registration.
  *   - `domain.key`  — the certificate's private key. Bundled into the keystore but also
  *     written separately so external tools (nginx, custom proxies) can read it.
  *   - `keystore.jks` — JKS keystore containing `domain.key` plus the issued cert chain,
  *     under the alias [[AcmeConfig.primaryDomain]]. The Spice HTTPS listener reads this.
  *
  * RSA 2048 is used for both keys — same default as acme4j's helpers and well within
  * Let's Encrypt's accepted algorithms. ECDSA P-256 is also valid; left as future work
  * if shorter-handshake performance matters. */
object AcmeKeyStore {
  private val KeySize: Int = 2048

  /** Load `file` as a PEM-encoded RSA key pair, or generate one and write it if missing. */
  def loadOrCreateKeyPair(file: File): KeyPair = synchronized {
    if (file.exists() && file.length() > 0) {
      val reader = new FileReader(file)
      try KeyPairUtils.readKeyPair(reader)
      finally reader.close()
    } else {
      ensureParentDir(file)
      val pair = KeyPairUtils.createKeyPair(KeySize)
      val writer = new FileWriter(file)
      try KeyPairUtils.writeKeyPair(pair, writer)
      finally writer.close()
      // Restrict to owner-read where the filesystem supports it.
      try file.setReadable(false, false) finally ()
      try file.setReadable(true, true) finally ()
      pair
    }
  }

  /** Build a JKS keystore containing `domainKey` + `chain` under `alias` and write to `file`. */
  def writeKeystore(file: File,
                    password: String,
                    alias: String,
                    domainKey: KeyPair,
                    chain: Array[X509Certificate]): Unit = synchronized {
    require(chain.nonEmpty, "Certificate chain must contain at least the leaf cert")
    ensureParentDir(file)
    val ks = KeyStore.getInstance("JKS")
    ks.load(null, password.toCharArray)
    ks.setKeyEntry(alias, domainKey.getPrivate, password.toCharArray, chain.asInstanceOf[Array[java.security.cert.Certificate]])
    val out = Files.newOutputStream(file.toPath)
    try ks.store(out, password.toCharArray)
    finally out.close()
  }

  /** Load the JKS at `file` (returning `None` if missing). */
  def loadKeystore(file: File, password: String): Option[KeyStore] = {
    if (!file.exists() || file.length() == 0) None
    else {
      val ks = KeyStore.getInstance("JKS")
      val in = Files.newInputStream(file.toPath)
      try ks.load(in, password.toCharArray)
      finally in.close()
      Some(ks)
    }
  }

  /** `notAfter` instant of the leaf certificate at `alias` in the keystore at `file`,
    * or `None` if the file doesn't exist, the alias is missing, or the stored cert isn't
    * an X509 certificate. */
  def certNotAfter(file: File, password: String, alias: String): Option[Instant] = {
    loadKeystore(file, password).flatMap { ks =>
      Option(ks.getCertificate(alias)).collect {
        case x: X509Certificate => x.getNotAfter.toInstant
      }
    }
  }

  /** True when the cert is expired or within `threshold` of expiring. */
  def needsRenewal(notAfter: Instant, threshold: FiniteDuration, now: Instant = Instant.now()): Boolean = {
    val cutoff = now.plusMillis(threshold.toMillis)
    !notAfter.isAfter(cutoff)
  }

  private def ensureParentDir(file: File): Unit = {
    val parent = file.getParentFile
    if (parent != null && !parent.exists()) parent.mkdirs()
  }

  // SecureRandom isn't strictly needed (KeyPairUtils handles its own RNG) but keeping a
  // referenced instance prevents a future maintainer from importing the unsafe default.
  @SuppressWarnings(Array("unused"))
  private val _rng: SecureRandom = new SecureRandom()
}
