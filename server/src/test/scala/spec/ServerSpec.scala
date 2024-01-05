package spec

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fabric.io.JsonParser
import fabric.obj
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import scribe.mdc.MDC
import spice.ValidationError
import spice.http.content.{Content, FormDataContent, JsonContent, StringContent}
import spice.http.server.dsl._
import spice.http.server.handler.HttpHandler
import spice.http.{HttpExchange, HttpMethod, HttpRequest, HttpStatus, paths}
import spice.http.server.{HttpServer, MutableHttpServer}
import spice.http.server.rest.{MultipartRequest, Restful, RestfulResponse}
import spice.net._

import java.io.File

class ServerSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  object server extends MutableHttpServer

  "TestHttpApplication" should {
    "configure the TestServer" in {
      server.handler.matcher(paths.exact("/test.html")).wrap(new HttpHandler {
        override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = {
          exchange.modify { response =>
            IO(response.withContent(Content.string("test!", ContentType.`text/plain`)))
          }
        }
      })
      server.handlers.size should be(1)
    }
    "configure Restful endpoints" in {
      server.handler(
        filters(
          path"/test/reverse" / ReverseService,
          path"/test/reverse/:value" / ReverseService,
          path"/test/time" / ServerTimeService,
          path"/test/file" / FileUploadService
        )
      )
      server.handlers.size should be(2)
    }
    "receive OK for test.html" in {
      server.handle(HttpExchange(HttpRequest(url = url"http://localhost/test.html"))).map { exchange =>
        exchange.response.status should be(HttpStatus.OK)
      }
    }
    "receive NotFound for other.html" in {
      server.handle(HttpExchange(HttpRequest(url = url"http://localhost/other.html"))).map { exchange =>
        exchange.response.status should be(HttpStatus.NotFound)
      }
    }
    "reverse a String with the Restful endpoint via POST" in {
      val content = Content.json(ReverseRequest("Testing").json)
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Post,
        url = url"http://localhost/test/reverse",
        content = Some(content)
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.OK)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val response = json.as[ReverseResponse]
        response.errors should be(Nil)
        response.reversed should be(Some("gnitseT"))
      }
    }
    "reverse a String with the Restful endpoint via GET" in {
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Get,
        url = url"http://localhost/test/reverse?value=Testing"
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.OK)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val response = json.as[ReverseResponse]
        response.errors should be(Nil)
        response.reversed should be(Some("gnitseT"))
      }
    }
    "reverse a String with the Restful endpoint via GET with path-based arg" in {
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Get,
        url = url"http://localhost/test/reverse/Testing"
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.OK)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val response = json.as[ReverseResponse]
        response.errors should be(Nil)
        response.reversed should be(Some("gnitseT"))
      }
    }
    "call a Restful endpoint that takes Unit as the request" in {
      val begin = System.currentTimeMillis()
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Get,
        url = url"http://localhost/test/time"
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.OK)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val response = json.as[Long]
        response should be >= begin
      }
    }
    "call a Restful endpoint that takes multipart content" in {
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Post,
        url = url"http://localhost/test/file",
        content = Some(FormDataContent
          .withJson("request", FileInfo("test.png", "Test Image").json)
          .withFile("image", "test.png", new File("test.png"))
        )
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.OK)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val fileName = json.as[String]
        fileName should be("test.png")
      }
    }
  }

  case class ReverseRequest(value: String)

  object ReverseRequest {
    implicit val rw: RW[ReverseRequest] = RW.gen
  }

  case class ReverseResponse(reversed: Option[String], errors: List[ValidationError])

  object ReverseResponse {
    implicit val rw: RW[ReverseResponse] = RW.gen
  }

  object ReverseService extends Restful[ReverseRequest, ReverseResponse] {
    override def apply(exchange: HttpExchange, request: ReverseRequest)
                      (implicit mdc: MDC): IO[RestfulResponse[ReverseResponse]] = {
      IO.pure(RestfulResponse(ReverseResponse(Some(request.value.reverse), Nil), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[ReverseResponse] = {
      RestfulResponse(ReverseResponse(None, errors), status)
    }
  }

  object ServerTimeService extends Restful[Unit, Long] {
    override def apply(exchange: HttpExchange, request: Unit)(implicit mdc: MDC): IO[RestfulResponse[Long]] = {
      IO.pure(RestfulResponse(System.currentTimeMillis(), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Long] = {
      RestfulResponse(0L, status)
    }
  }

  object FileUploadService extends Restful[FileInfo, String] {
    override def apply(exchange: HttpExchange,
                       request: FileInfo)
                      (implicit mdc: MDC): IO[RestfulResponse[String]] = IO {
      request.content should not be None
      val content = request.content
      val fileEntry = content.getFile("image")
      assert(fileEntry.file.length() == 33404)
      ok(request.fileName)
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[String] = RestfulResponse("Failure!", HttpStatus.InternalServerError)
  }

  case class FileInfo(fileName: String, description: String, content: FormDataContent = FormDataContent) extends MultipartRequest {
    override def withContent(content: FormDataContent): MultipartRequest = copy(content = content)
  }

  object FileInfo {
    implicit val rw: RW[FileInfo] = RW.gen
  }
}