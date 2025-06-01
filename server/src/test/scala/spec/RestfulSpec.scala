package spec

import fabric._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.server.rest.Restful
import spice.net._

class RestfulSpec extends AnyWordSpec with Matchers {
  "Restful" when {
    "working with GET parameters" should {
      "convert dot-separated values properly" in {
        val url = url"https://somewhere.com?foo.one=1&foo.two=2"
        val json = Restful.jsonFromURL(url)
        json should be(obj(
          "foo" -> obj(
            "one" -> str("1"),
            "two" -> str("2")
          )
        ))
      }
    }
  }
}
