package spice.http.server

import java.io.File
import java.net.Socket
import java.nio.file.Files
import java.security.KeyStore as JKeyStore
import java.security.cert.X509Certificate
import java.security.{Principal, PrivateKey}
import java.util.concurrent.atomic.AtomicReference
import javax.net.ssl.*

/** A swappable [[X509ExtendedKeyManager]] backed by an [[AtomicReference]].
  *
  * SSL/TLS in Java pins the [[KeyManager]] array at [[SSLContext#init]] time — there is no
  * built-in way to swap a cert in a running server. This wrapper exposes a stable
  * `KeyManager` whose internal delegate can be replaced atomically. New TLS handshakes
  * after a `reload(...)` call use the new cert; in-flight TLS connections (already past
  * handshake) are unaffected because TLS uses session keys derived during handshake, not
  * the certificate itself. Long-lived WebSocket and HTTP/2 connections opened against the
  * old cert continue working until they close on their own terms.
  *
  * The wrapper extends `X509ExtendedKeyManager` rather than `X509KeyManager` so the JDK's
  * SNI-aware code paths (which downcast to `X509ExtendedKeyManager`) work correctly. */
final class ReloadableX509KeyManager(initial: X509ExtendedKeyManager) extends X509ExtendedKeyManager {
  private val ref = new AtomicReference[X509ExtendedKeyManager](initial)

  /** Replace the inner KeyManager. Returns immediately; new handshakes after this point
    * will use `next`. */
  def reload(next: X509ExtendedKeyManager): Unit = ref.set(next)

  private def km: X509ExtendedKeyManager = ref.get()

  override def getClientAliases(keyType: String, issuers: Array[Principal]): Array[String] =
    km.getClientAliases(keyType, issuers)
  override def chooseClientAlias(keyType: Array[String], issuers: Array[Principal], socket: Socket): String =
    km.chooseClientAlias(keyType, issuers, socket)
  override def getServerAliases(keyType: String, issuers: Array[Principal]): Array[String] =
    km.getServerAliases(keyType, issuers)
  override def chooseServerAlias(keyType: String, issuers: Array[Principal], socket: Socket): String =
    km.chooseServerAlias(keyType, issuers, socket)
  override def getCertificateChain(alias: String): Array[X509Certificate] =
    km.getCertificateChain(alias)
  override def getPrivateKey(alias: String): PrivateKey =
    km.getPrivateKey(alias)
  override def chooseEngineClientAlias(keyType: Array[String], issuers: Array[Principal], engine: SSLEngine): String =
    km.chooseEngineClientAlias(keyType, issuers, engine)
  override def chooseEngineServerAlias(keyType: String, issuers: Array[Principal], engine: SSLEngine): String =
    km.chooseEngineServerAlias(keyType, issuers, engine)
}

/** Builds an `SSLContext` whose underlying `X509ExtendedKeyManager` can be hot-swapped
  * via the returned [[ReloadableX509KeyManager]].
  *
  * Typical use:
  * {{{
  *   val (sslContext, reloader) = ReloadableSSLContext.fromKeystore(keystoreFile, password)
  *   // ... hand sslContext to Undertow ...
  *   // Later, after the cert was rewritten on disk:
  *   reloader.reloadFrom(keystoreFile, password)
  * }}}
  */
object ReloadableSSLContext {
  /** Create a fresh reloadable `SSLContext` from the JKS at `keystoreFile`.
    * The returned `Reloader` exposes `reloadFrom(File, String)` for swapping in a new
    * keystore atomically. */
  def fromKeystore(keystoreFile: File, password: String): (SSLContext, Reloader) = {
    val initial = readKeyManager(keystoreFile, password)
    val reloadable = new ReloadableX509KeyManager(initial)

    val ks = loadKeyStore(keystoreFile, password)
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(ks)

    val ctx = SSLContext.getInstance("TLS")
    ctx.init(Array(reloadable), tmf.getTrustManagers, null)
    (ctx, new Reloader(reloadable))
  }

  /** Handle returned by [[fromKeystore]] — call `reloadFrom` to swap in a fresh cert. */
  final class Reloader private[ReloadableSSLContext] (km: ReloadableX509KeyManager) {
    /** Read the keystore at `file` and atomically replace the SSLContext's KeyManager.
      * Throws if the file is missing or unreadable; on success the SSLContext continues
      * to be used by every existing listener — no rebuild needed. */
    def reloadFrom(file: File, password: String): Unit = {
      val next = readKeyManager(file, password)
      km.reload(next)
    }
  }

  private def readKeyManager(file: File, password: String): X509ExtendedKeyManager = {
    val ks = loadKeyStore(file, password)
    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    kmf.init(ks, password.toCharArray)
    kmf.getKeyManagers.collectFirst { case x: X509ExtendedKeyManager => x }.getOrElse(
      throw new IllegalStateException(s"KeyManagerFactory did not return an X509ExtendedKeyManager for $file")
    )
  }

  private def loadKeyStore(file: File, password: String): JKeyStore = {
    val ks = JKeyStore.getInstance("JKS")
    val in = Files.newInputStream(file.toPath)
    try ks.load(in, password.toCharArray)
    finally in.close()
    ks
  }
}
