package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.server.ReloadableX509KeyManager

import java.net.Socket
import java.security.cert.X509Certificate
import java.security.{Principal, PrivateKey}
import javax.net.ssl.{SSLEngine, X509ExtendedKeyManager}

/** Confirms that `ReloadableX509KeyManager` actually delegates to whatever was last set
  * via `reload`. The full SSLContext factory needs a real JKS to exercise (covered in
  * an integration test against Pebble; out of scope for this unit suite), but the swap
  * mechanic itself can be verified with stub `X509ExtendedKeyManager` instances. */
class ReloadableSSLContextSpec extends AnyWordSpec with Matchers {
  private def tagged(tag: String): X509ExtendedKeyManager = new X509ExtendedKeyManager {
    override def getClientAliases(keyType: String, issuers: Array[Principal]): Array[String] = Array(s"client-$tag")
    override def chooseClientAlias(keyType: Array[String], issuers: Array[Principal], socket: Socket): String = s"client-$tag"
    override def getServerAliases(keyType: String, issuers: Array[Principal]): Array[String] = Array(s"server-$tag")
    override def chooseServerAlias(keyType: String, issuers: Array[Principal], socket: Socket): String = s"server-$tag"
    override def getCertificateChain(alias: String): Array[X509Certificate] = Array.empty
    override def getPrivateKey(alias: String): PrivateKey = null
    override def chooseEngineClientAlias(keyType: Array[String], issuers: Array[Principal], engine: SSLEngine): String = s"engine-client-$tag"
    override def chooseEngineServerAlias(keyType: String, issuers: Array[Principal], engine: SSLEngine): String = s"engine-server-$tag"
  }

  "ReloadableX509KeyManager" should {
    "delegate to the initial inner KeyManager" in {
      val initial = tagged("v1")
      val km = new ReloadableX509KeyManager(initial)
      km.chooseServerAlias("RSA", Array.empty, null) shouldBe "server-v1"
      km.chooseEngineServerAlias("RSA", Array.empty, null) shouldBe "engine-server-v1"
    }

    "swap to the next inner KeyManager after reload" in {
      val km = new ReloadableX509KeyManager(tagged("v1"))
      km.chooseServerAlias("RSA", Array.empty, null) shouldBe "server-v1"
      km.reload(tagged("v2"))
      km.chooseServerAlias("RSA", Array.empty, null) shouldBe "server-v2"
      km.chooseEngineServerAlias("RSA", Array.empty, null) shouldBe "engine-server-v2"
    }

    "be safe to swap repeatedly" in {
      val km = new ReloadableX509KeyManager(tagged("v1"))
      (1 to 50).foreach(i => km.reload(tagged(s"v$i")))
      km.chooseServerAlias("RSA", Array.empty, null) shouldBe "server-v50"
    }
  }
}
