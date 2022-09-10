package spec

import moduload.Moduload
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.client.{HttpClient, OkHttpClientImplementation}

class OkHttpClientSpec extends AnyWordSpec with Matchers {
  "OkHttpClient" should {
    "load Moduload" in {
      Moduload.load()
    }
    "be the default implementation" in {
      HttpClient.implementation.getClass should be(classOf[OkHttpClientImplementation])
    }
  }
}
