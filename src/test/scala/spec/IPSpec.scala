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
  }
}