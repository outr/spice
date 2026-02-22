package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.{Header, HeaderKey, Headers, StringHeaderKey}
import spice.net.ContentType

class HeadersSpec extends AnyWordSpec with Matchers {
  "Headers" should {
    "start empty with no entries" in {
      val headers = Headers.empty
      headers.map should be(empty)
    }
    "support case-insensitive header access" in {
      val headers = Headers.empty.withHeader("Content-Type", "text/html")
      val lowerKey = new StringHeaderKey("content-type")
      val upperKey = new StringHeaderKey("CONTENT-TYPE")
      val mixedKey = new StringHeaderKey("Content-Type")
      headers.first(lowerKey) should be(Some("text/html"))
      headers.first(upperKey) should be(Some("text/html"))
      headers.first(mixedKey) should be(Some("text/html"))
    }
    "add headers with withHeader appending to existing values" in {
      val key = new StringHeaderKey("X-Custom")
      val headers = Headers.empty
        .withHeader(key("value1"))
        .withHeader(key("value2"))
      headers.get(key) should be(List("value1", "value2"))
    }
    "add headers with withHeader using key/value strings" in {
      val headers = Headers.empty.withHeader("X-Test", "hello")
      val key = new StringHeaderKey("X-Test")
      headers.first(key) should be(Some("hello"))
    }
    "set headers with setHeader replacing existing values" in {
      val key = new StringHeaderKey("X-Custom")
      val headers = Headers.empty
        .withHeader(key("value1"))
        .withHeader(key("value2"))
        .setHeader(key("replaced"))
      headers.get(key) should be(List("replaced"))
    }
    "remove headers with removeHeader" in {
      val key = new StringHeaderKey("X-Custom")
      val headers = Headers.empty
        .withHeader(key("value1"))
        .removeHeader(key)
      headers.get(key) should be(Nil)
      headers.contains(key) should be(false)
    }
    "merge two Headers instances" in {
      val key1 = new StringHeaderKey("X-First")
      val key2 = new StringHeaderKey("X-Second")
      val headers1 = Headers.empty.withHeader(key1("one"))
      val headers2 = Headers.empty.withHeader(key2("two"))
      val merged = headers1.merge(headers2)
      merged.first(key1) should be(Some("one"))
      merged.first(key2) should be(Some("two"))
    }
    "overwrite values on merge when keys overlap" in {
      val key = new StringHeaderKey("X-Overlap")
      val headers1 = Headers.empty.withHeader(key("original"))
      val headers2 = Headers.empty.withHeader(key("overwritten"))
      val merged = headers1.merge(headers2)
      merged.first(key) should be(Some("overwritten"))
    }
    "parse Content-Type header via typed header key" in {
      val headers = Headers.empty.withHeader(Headers.`Content-Type`(ContentType.`text/html`))
      val ct = Headers.`Content-Type`.value(headers)
      ct shouldBe defined
      ct.get.`type` should be("text")
      ct.get.subType should be("html")
    }
    "parse Content-Type with charset via typed header key" in {
      val headers = Headers.empty.withHeader(Headers.`Content-Type`(ContentType.`text/plain`.withCharSet("UTF-8")))
      val ct = Headers.`Content-Type`.value(headers)
      ct shouldBe defined
      ct.get.`type` should be("text")
      ct.get.subType should be("plain")
    }
    "return None for Content-Type when not present" in {
      val headers = Headers.empty
      Headers.`Content-Type`.value(headers) should be(None)
    }
    "check header presence with contains" in {
      val key = new StringHeaderKey("X-Present")
      val headers = Headers.empty.withHeader(key("yes"))
      headers.contains(key) should be(true)
      val absent = new StringHeaderKey("X-Absent")
      headers.contains(absent) should be(false)
    }
    "return Nil for get on missing header" in {
      val key = new StringHeaderKey("X-Missing")
      Headers.empty.get(key) should be(Nil)
    }
    "return None for first on missing header" in {
      val key = new StringHeaderKey("X-Missing")
      Headers.empty.first(key) should be(None)
    }
    "add multiple headers at once with withHeaders" in {
      val key1 = new StringHeaderKey("X-A")
      val key2 = new StringHeaderKey("X-B")
      val headers = Headers.empty.withHeaders(key1("alpha"), key2("beta"))
      headers.first(key1) should be(Some("alpha"))
      headers.first(key2) should be(Some("beta"))
    }
    "create Headers from a Map" in {
      val headers = Headers(Map("X-One" -> List("1"), "X-Two" -> List("2a", "2b")))
      val key1 = new StringHeaderKey("X-One")
      val key2 = new StringHeaderKey("X-Two")
      headers.first(key1) should be(Some("1"))
      headers.get(key2) should be(List("2a", "2b"))
    }
  }
}
