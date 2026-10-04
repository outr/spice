package spec

import moduload.Moduload
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.*
import spice.http.client.HttpClient
import spice.http.content.Content
import spice.net.*

import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*

class JVMContentLengthSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private lazy val server = new HeaderCaptureServer

  override protected def beforeAll(): Unit = Moduload.load()

  override protected def afterAll(): Unit = server.close()

  private def send(method: HttpMethod, content: Option[Content] = None): HeaderCaptureServer.Captured = {
    HttpClient
      .timeout(10.seconds)
      .url(URL.parse(s"http://127.0.0.1:${server.port}/capture"))
      .method(method)
      .content(content)
      .noFailOnHttpStatus
      .send()
      .sync()
    server.next()
  }

  "a request with no content" should {
    "send no Content-Length or Transfer-Encoding on a GET" in {
      val captured = send(HttpMethod.Get)
      captured.method shouldBe "GET"
      captured.contentLength shouldBe None
      captured.transferEncoding shouldBe None
    }
    "send no Content-Length or Transfer-Encoding on a HEAD" in {
      val captured = send(HttpMethod.Head)
      captured.method shouldBe "HEAD"
      captured.contentLength shouldBe None
      captured.transferEncoding shouldBe None
    }
    "send no Content-Length or Transfer-Encoding on a DELETE" in {
      val captured = send(HttpMethod.Delete)
      captured.method shouldBe "DELETE"
      captured.contentLength shouldBe None
      captured.transferEncoding shouldBe None
    }
    "send no Content-Length or Transfer-Encoding on an OPTIONS" in {
      val captured = send(HttpMethod.Options)
      captured.method shouldBe "OPTIONS"
      captured.contentLength shouldBe None
      captured.transferEncoding shouldBe None
    }
    "send Content-Length: 0 on a POST" in {
      val captured = send(HttpMethod.Post)
      captured.method shouldBe "POST"
      captured.contentLength shouldBe Some(List("0"))
      captured.transferEncoding shouldBe None
    }
  }

  "a request with content" should {
    "send Content-Length: 0 on a POST with an empty body" in {
      val captured = send(HttpMethod.Post, Some(Content.string("", ContentType.`text/plain`)))
      captured.method shouldBe "POST"
      captured.contentLength shouldBe Some(List("0"))
      captured.transferEncoding shouldBe None
    }
    "send its length on a POST with a body" in {
      val body = "héllo, wörld"
      val captured = send(HttpMethod.Post, Some(Content.string(body, ContentType.`text/plain`)))
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      captured.method shouldBe "POST"
      captured.contentLength shouldBe Some(List(bytes.length.toString))
      captured.transferEncoding shouldBe None
      captured.body.toList shouldBe bytes.toList
    }
    "send its length on a PUT with a body" in {
      val captured = send(HttpMethod.Put, Some(Content.string("abc", ContentType.`text/plain`)))
      captured.method shouldBe "PUT"
      captured.contentLength shouldBe Some(List("3"))
    }
  }
}
