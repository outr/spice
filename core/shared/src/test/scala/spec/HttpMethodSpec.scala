package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.HttpMethod

class HttpMethodSpec extends AnyWordSpec with Matchers {
  "HttpMethod" should {
    "have standard methods defined" in {
      HttpMethod.Get should not be null
      HttpMethod.Post should not be null
      HttpMethod.Put should not be null
      HttpMethod.Delete should not be null
      HttpMethod.Patch should not be null
      HttpMethod.Options should not be null
      HttpMethod.Head should not be null
      HttpMethod.Trace should not be null
      HttpMethod.Connect should not be null
    }
    "return the correct string value for each method" in {
      HttpMethod.Get.value should be("GET")
      HttpMethod.Post.value should be("POST")
      HttpMethod.Put.value should be("PUT")
      HttpMethod.Delete.value should be("DELETE")
      HttpMethod.Patch.value should be("PATCH")
      HttpMethod.Options.value should be("OPTIONS")
      HttpMethod.Head.value should be("HEAD")
      HttpMethod.Trace.value should be("TRACE")
      HttpMethod.Connect.value should be("CONNECT")
    }
    "use value as toString representation" in {
      HttpMethod.Get.toString should be("GET")
      HttpMethod.Post.toString should be("POST")
    }
    "look up methods case-insensitively via get" in {
      HttpMethod.get("get") should be(Some(HttpMethod.Get))
      HttpMethod.get("GET") should be(Some(HttpMethod.Get))
      HttpMethod.get("Get") should be(Some(HttpMethod.Get))
      HttpMethod.get("post") should be(Some(HttpMethod.Post))
      HttpMethod.get("POST") should be(Some(HttpMethod.Post))
      HttpMethod.get("delete") should be(Some(HttpMethod.Delete))
    }
    "look up methods case-insensitively via apply" in {
      HttpMethod("get") should be(HttpMethod.Get)
      HttpMethod("PUT") should be(HttpMethod.Put)
      HttpMethod("Patch") should be(HttpMethod.Patch)
    }
    "return None for unknown methods via get" in {
      HttpMethod.get("UNKNOWN") should be(None)
    }
    "throw an exception for unknown methods via apply" in {
      an[Exception] should be thrownBy HttpMethod("UNKNOWN")
    }
  }
}
