package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.net.{DNS, IP}

class DNSSpec extends AnyWordSpec with Matchers {
  "DNS" should {
    "preserve IPv6 literals instead of truncating them to IPv4" in {
      DNS.default.lookup("2606:4700:3031::ac43:d73e").sync().map(_.addressString) shouldBe
        Some("2606:4700:3031:0:0:0:ac43:d73e")
      DNS.default.lookup("::1").sync() shouldBe Some(IP.v6.LocalHost)
    }
    "retain high IPv4 octets" in {
      DNS.default.lookup("192.0.2.255").sync() shouldBe Some(IP.v4(192, 0, 2, 255))
    }
    "honor case-insensitive overrides and otherwise retain all system candidates" in {
      val dns = DNS(Map("Example.TEST" -> IP.v6.LocalHost))
      dns.lookupAll("example.test").sync() shouldBe List(IP.v6.LocalHost)
      dns.lookupAll("localhost").sync() shouldBe DNS.default.lookupAll("localhost").sync()
    }
  }
}
