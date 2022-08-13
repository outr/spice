package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import spice.net._
import fabric._
import fabric.rw._

class PortSpec extends AnyWordSpec with Matchers {
  "Port" should {
    "validate 8080 via interpolation" in {
      val port = port"8080"
      port.value should be(8080)
    }
    "fail validation of -1 via interpolation" in {
      assertDoesNotCompile("""port"-1"""")
    }
    "fail validation of 100000 via interpolation" in {
      assertDoesNotCompile("""port"100000"""")
    }
    "convert to JSON" in {
      val port = port"123"
      port.json should be(num(123))
    }
    "convert from JSON" in {
      num(123).as[Port] should be(Port.fromInt(123).get)
    }
  }
}