package spice.http.server

import java.io.File
import java.nio.file.Files
import java.security.KeyStore
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory, X509ExtendedKeyManager}

object SSLUtil {
  /** Build an `SSLContext` from a JKS keystore. The returned context's KeyManagers are
    * fixed for its lifetime — to support hot cert reload, use [[createReloadableSSLContext]]
    * instead. */
  def createSSLContext(keyStoreFile: File, password: String): SSLContext = {
    val (km, tm) = readKeyAndTrustManagers(keyStoreFile, password)
    val ctx = SSLContext.getInstance("TLS")
    ctx.init(km.toArray, tm, null)
    ctx
  }

  /** Build a reloadable `SSLContext`: the inner `X509ExtendedKeyManager` is wrapped in a
    * [[ReloadableX509KeyManager]] so the cert can be swapped atomically via the returned
    * `Reloader`. The same `SSLContext` instance can be handed to a running Undertow
    * listener and continue serving across cert renewals — no listener teardown. */
  def createReloadableSSLContext(keyStoreFile: File, password: String): (SSLContext, ReloadableSSLContext.Reloader) =
    ReloadableSSLContext.fromKeystore(keyStoreFile, password)

  private def readKeyAndTrustManagers(keyStoreFile: File,
                                      password: String): (List[X509ExtendedKeyManager], Array[javax.net.ssl.TrustManager]) = {
    val passwordChars = password.toCharArray
    val keyStore = KeyStore.getInstance("JKS")
    assert(keyStoreFile.exists(), s"No keystore file was found at the location: ${keyStoreFile.getAbsolutePath}")
    val keyStoreInput = Files.newInputStream(keyStoreFile.toPath)
    try keyStore.load(keyStoreInput, passwordChars)
    finally keyStoreInput.close()

    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    kmf.init(keyStore, passwordChars)
    val keyManagers = kmf.getKeyManagers.toList.collect { case x: X509ExtendedKeyManager => x }

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    tmf.init(keyStore)

    (keyManagers, tmf.getTrustManagers)
  }
}
