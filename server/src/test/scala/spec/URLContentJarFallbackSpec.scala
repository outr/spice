package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.content.URLContent
import spice.http.server.dsl.ClassLoaderPath
import spice.net.ContentType

import java.io.{ByteArrayInputStream, InputStream}
import java.net.{URL, URLConnection, URLStreamHandler}

/** Reproduces the Cloud-Run-only bug: classloader resources packaged inside a jar can
  * report `getContentLengthLong = -1`, which historically leaked through to the wire as
  * `Content-Length: -1`. HTTP/1.1 frontends tolerate it; HTTP/2 frontends (Cloud Run's
  * GFE, Envoy-based proxies) reject the response framing and return 502.
  *
  * We can't easily build a real jar URL whose `getContentLengthLong` returns `-1` — that
  * behavior depends on JVM/jar specifics. Instead we install a fake `URLStreamHandler`
  * that returns `-1` deterministically and verify URLContent eagerly reads the bytes
  * and reports the actual length. */
class URLContentJarFallbackSpec extends AnyWordSpec with Matchers {
  private def fakeUrl(payload: Array[Byte], reportLength: Long): URL = {
    val handler = new URLStreamHandler {
      override def openConnection(u: URL): URLConnection = new URLConnection(u) {
        override def connect(): Unit = ()
        override def getContentLengthLong: Long = reportLength
        override def getLastModified: Long = 0L
        override def getInputStream: InputStream = new ByteArrayInputStream(payload)
      }
    }
    new URL(null, "fake://test/resource", handler)
  }

  "URLContent" should {
    "report the streamed length when the URL connection knows it (happy path)" in {
      val payload = "hello".getBytes("UTF-8")
      val url = fakeUrl(payload, reportLength = payload.length.toLong)
      val content = URLContent(url, ContentType.`text/plain`)
      content.length shouldBe 5L
    }

    "fall back to reading bytes when the connection reports -1 (jar-resource bug)" in {
      val payload = "hello world".getBytes("UTF-8")
      val url = fakeUrl(payload, reportLength = -1L)
      val content = URLContent(url, ContentType.`text/plain`)
      // Without the fallback, this returned -1 and SenderHandler emitted Content-Length: -1.
      content.length shouldBe payload.length.toLong
    }

    "serve cached bytes via asString after a -1 fallback" in {
      val payload = "hello world".getBytes("UTF-8")
      val url = fakeUrl(payload, reportLength = -1L)
      val content = URLContent(url, ContentType.`text/plain`)
      // Force the fallback path
      content.length shouldBe payload.length.toLong
      content.asString.sync() shouldBe "hello world"
    }

    "never expose a negative length on the wire" in {
      // Defensive guard for any future regression: regardless of what the connection
      // says, URLContent.length must always be >= 0 because Content-Length: <neg> is
      // rejected by HTTP/2 strict frontends.
      val payload = Array.fill(42)(0xAB.toByte)
      val url = fakeUrl(payload, reportLength = -1L)
      val content = URLContent(url, ContentType.`application/octet-stream`)
      content.length should be >= 0L
      content.length shouldBe 42L
    }
  }

  "ClassLoaderPath" should {
    "strip a trailing slash from classPathRoot without truncating to '/' (substring bug)" in {
      // Pre-fix: substring(classPathRoot.length - 1) returned only the LAST character,
      // turning "static/" into "/" and breaking every lookup. Post-fix: returns
      // "everything except the trailing slash".
      val withSlash = ClassLoaderPath("static/")
      val withoutSlash = ClassLoaderPath("static")
      // Both should resolve identically — same private `dir` value. We assert via an
      // observable: the case-class equality on classPathRoot is preserved (no internal
      // mutation), and the resolved internal `dir` is reflected via the toString of
      // the case class. Use reflective field access for the canonical assertion.
      val dirField = classOf[ClassLoaderPath].getDeclaredField("dir")
      dirField.setAccessible(true)
      dirField.get(withSlash) shouldBe "static"
      dirField.get(withoutSlash) shouldBe "static"
    }
  }
}
