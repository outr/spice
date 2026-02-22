package spec

import rapid.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.{HttpExchange, HttpRequest, HttpResponse, HttpStatus, Headers}
import spice.http.server.DefaultErrorHandler
import spice.http.content.{Content, StringContent, JsonContent}
import spice.net.*

class ErrorHandlerSpec extends AnyWordSpec with Matchers {
  private def exchange(status: HttpStatus, headers: Headers = Headers.empty): HttpExchange = {
    val request = HttpRequest(headers = headers)
    HttpExchange(
      request = request,
      response = HttpResponse(status = status),
      path = URLPath.empty,
      store = new spice.store.MapStore(),
      finished = false
    )
  }

  "DefaultErrorHandler" when {
    "generating HTML error responses" should {
      "produce HTML content for NotFound status" in {
        val ex = exchange(HttpStatus.NotFound)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val content = result.response.content
        content shouldBe defined
        val str = content.get.asString.sync()
        str should include("404")
        str should include("Not Found")
        content.get.contentType should be(ContentType.`text/html`)
      }

      "produce HTML content for InternalServerError status" in {
        val ex = exchange(HttpStatus.InternalServerError)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val content = result.response.content
        content shouldBe defined
        val str = content.get.asString.sync()
        str should include("500")
        str should include("Internal Server Error")
      }

      "produce HTML content for Forbidden status" in {
        val ex = exchange(HttpStatus.Forbidden)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val content = result.response.content
        content shouldBe defined
        val str = content.get.asString.sync()
        str should include("403")
        str should include("Forbidden")
      }

      "produce HTML content for BadRequest status" in {
        val ex = exchange(HttpStatus.BadRequest)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val content = result.response.content
        content shouldBe defined
        val str = content.get.asString.sync()
        str should include("400")
        str should include("Bad Request")
      }

      "produce proper HTML structure with title and body" in {
        val ex = exchange(HttpStatus.NotFound)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val str = result.response.content.get.asString.sync()
        str should include("<html>")
        str should include("<title>Error 404</title>")
        str should include("</html>")
      }

      "default to InternalServerError for non-error status codes" in {
        val ex = exchange(HttpStatus.OK)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val content = result.response.content
        content shouldBe defined
        val str = content.get.asString.sync()
        str should include("500")
        str should include("Internal Server Error")
      }
    }

    "generating JSON error responses" should {
      "produce JSON content when Accept header contains application/json" in {
        val headers = Headers.empty.withHeader(Headers.Request.`Accept`("application/json"))
        val ex = exchange(HttpStatus.NotFound, headers)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val content = result.response.content
        content shouldBe defined
        content.get shouldBe a[JsonContent]
        val str = content.get.asString.sync()
        str should include("404")
        str should include("Not Found")
      }

      "produce HTML content when Accept header contains */*" in {
        val headers = Headers.empty.withHeader(Headers.Request.`Accept`("*/*"))
        val ex = exchange(HttpStatus.NotFound, headers)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val content = result.response.content
        content shouldBe defined
        content.get.contentType should be(ContentType.`text/html`)
      }

      "include throwable message in JSON response when provided" in {
        val headers = Headers.empty.withHeader(Headers.Request.`Accept`("application/json"))
        val ex = exchange(HttpStatus.InternalServerError, headers)
        val error = new RuntimeException("Something went wrong")
        val result = DefaultErrorHandler.handle(ex, Some(error)).sync()
        val str = result.response.content.get.asString.sync()
        str should include("Something went wrong")
        str should include("500")
      }

      "use status message as JSON message when no throwable is provided" in {
        val headers = Headers.empty.withHeader(Headers.Request.`Accept`("application/json"))
        val ex = exchange(HttpStatus.NotFound, headers)
        val result = DefaultErrorHandler.handle(ex, None).sync()
        val str = result.response.content.get.asString.sync()
        str should include("Not Found")
      }
    }

    "generating static HTML content" should {
      "produce consistent Content from the html method" in {
        val content = DefaultErrorHandler.html(HttpStatus.NotFound)
        content.contentType should be(ContentType.`text/html`)
        val str = content.asString.sync()
        str should include("404 - Not Found")
      }
    }

    "generating static JSON content" should {
      "produce valid JSON from the json method" in {
        val content = DefaultErrorHandler.json(HttpStatus.BadRequest)
        content shouldBe a[JsonContent]
        val str = content.asString.sync()
        str should include("400")
        str should include("Bad Request")
      }

      "include throwable message from the json method" in {
        val error = new RuntimeException("Custom error message")
        val content = DefaultErrorHandler.json(HttpStatus.InternalServerError, Some(error))
        val str = content.asString.sync()
        str should include("Custom error message")
      }
    }
  }
}
