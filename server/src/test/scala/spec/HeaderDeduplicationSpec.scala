package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scribe.mdc.MDC
import spice.http.content.{Content, StringContent}
import spice.http.server.handler.{CachingManager, SenderHandler}
import spice.http.{Headers, HttpExchange, HttpRequest, HttpStatus}
import spice.net.{ContentType, URL}

import java.io.File
import java.nio.file.Files

/** Locks in the rule that single-value response headers (Content-Length, Content-Type,
  * Cache-Control, ETag, etc.) are SET, not APPENDED, by handlers and middleware. The
  * historical bug: `SenderHandler` paired `withContent(...)` (which sets Content-Length)
  * with `withHeader(Headers.\`Content-Length\`(...))` (which appends), producing
  * `Content-Length: N, N` on the wire — clients per RFC 7230 §3.3.2 treat that as
  * malformed and may strip the header entirely. */
class HeaderDeduplicationSpec extends AnyWordSpec with Matchers {
  private given MDC = MDC.global

  private def baseExchange: HttpExchange =
    HttpExchange(HttpRequest(url = URL.parse("http://example.com/x")))

  "SenderHandler" should {
    "set Content-Length exactly once for StringContent" in {
      val content = StringContent("hello", ContentType.`text/plain`)
      val ex = SenderHandler.handle(baseExchange, content).sync()
      ex.response.headers.get(Headers.`Content-Length`) should have size 1
      ex.response.headers.first(Headers.`Content-Length`) shouldBe Some("5")
    }

    "set Content-Length exactly once for FileContent" in {
      val tmp = Files.createTempFile("spice-content-length-spec", ".bin").toFile
      try {
        Files.writeString(tmp.toPath, "x" * 1234)
        val ex = SenderHandler.handle(baseExchange, Content.file(tmp)).sync()
        ex.response.headers.get(Headers.`Content-Length`) should have size 1
        ex.response.headers.first(Headers.`Content-Length`) shouldBe Some("1234")
      } finally {
        val _ = tmp.delete()
      }
    }

    "honor an explicit length override without duplication" in {
      val content = StringContent("hello", ContentType.`text/plain`)
      val ex = SenderHandler.handle(baseExchange, content, length = Some(99L)).sync()
      ex.response.headers.get(Headers.`Content-Length`) should have size 1
      ex.response.headers.first(Headers.`Content-Length`) shouldBe Some("99")
    }
  }

  "CachingManager" should {
    "set Cache-Control exactly once on NotCached" in {
      val ex = CachingManager.NotCached.handle(baseExchange).sync()
      ex.response.headers.get(Headers.`Cache-Control`) should have size 1
    }

    "set Cache-Control exactly once on MaxAge" in {
      val ex = CachingManager.MaxAge(60).handle(baseExchange).sync()
      ex.response.headers.get(Headers.`Cache-Control`) should have size 1
    }

    "not duplicate Cache-Control when applied twice in a row" in {
      val first = CachingManager.MaxAge(60).handle(baseExchange).sync()
      val second = CachingManager.NotCached.handle(first).sync()
      second.response.headers.get(Headers.`Cache-Control`) should have size 1
      // setHeader replaces — the most recent decision wins.
      second.response.headers.first(Headers.`Cache-Control`).get should include("no-cache")
    }
  }

  "HttpResponse.withContent" should {
    "set Content-Length once even when called repeatedly with the same content type" in {
      val content = StringContent("hello", ContentType.`text/plain`)
      val r = baseExchange.response.withContent(content).withContent(content)
      r.headers.get(Headers.`Content-Length`) should have size 1
      r.headers.get(Headers.`Content-Type`) should have size 1
    }
  }
}
