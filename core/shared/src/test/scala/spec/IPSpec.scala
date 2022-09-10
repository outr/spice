package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import spice.net._

import fabric._
import fabric.rw._

class IPSpec extends AnyWordSpec with Matchers {
  "IP" should {
    "validate 127.0.0.1 via interpolation" in {
      val ip = ip"127.0.0.1"
      ip should be(IP.v4())
    }
    "fail interpolation of 127.0.0.1.2" in {
      assertDoesNotCompile("""ip"127.0.0.1.2"""")
    }
    "fail interpolation of 300.0.0.1" in {
      assertDoesNotCompile("""ip"300.0.0.1"""")
    }
    "convert to JSON" in {
      ip"255.255.255.0".json should be(str("255.255.255.0"))
    }
    "convert from JSON" in {
      str("255.255.255.0").as[IP] should be(IP.fromString("255.255.255.0").get)
    }
    "parsing IPv4 addresses" should {
      "properly parse 127.0.01" in {
        val ip = ip"127.0.0.1"
        ip should be(IP.v4())
      }
      "properly parse 1.2.3.4" in {
        val ip = ip"1.2.3.4"
        ip should be(IP.v4(1, 2, 3, 4))
      }
      "resolve None for 256.0.0.1" in {
        IP.fromString("256.0.0.1") should be(None)
      }
    }
    "parsing IPv6 addresses" should {
      "properly parse fe80:0:0:0:0:0:0:1%1" in {
        val ip = ip"fe80:0:0:0:0:0:0:1%1".asInstanceOf[IP.v6]
        ip.address should be(Vector(65152, 0, 0, 0, 0, 0, 0, 1))
        ip.scope should be(Some("1"))
        ip.addressString should be("fe80:0:0:0:0:0:0:1%1")
        ip.canonicalString should be("fe80:0000:0000:0000:0000:0000:0000:0001%1")
      }
      "properly parse 2604:ca00:129:99af::860:930a" in {
        val ip = ip"2604:ca00:129:99af::860:930a".asInstanceOf[IP.v6]
        ip.address should be(Vector(9732, 51712, 297, 39343, 0, 0, 2144, 37642))
        ip.scope should be(None)
        ip.addressString should be("2604:ca00:129:99af:0:0:860:930a")
        ip.canonicalString should be("2604:ca00:0129:99af:0000:0000:0860:930a")
      }
    }
  }
}