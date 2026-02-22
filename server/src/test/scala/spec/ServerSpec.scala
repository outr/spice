package spec

import rapid.*
import fabric.{Str, obj}
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.{AnyWordSpec, AsyncWordSpec}
import scribe.mdc.MDC
import spice.{ExceptionType, UserException, ValidationError}
import spice.http.content.{Content, FormDataContent, JsonContent, StringContent}
import spice.http.server.dsl.*
import spice.http.server.dsl.given
import spice.http.server.handler.HttpHandler
import spice.http.{HttpExchange, HttpMethod, HttpRequest, HttpStatus, paths}
import spice.http.server.{HttpServer, MutableHttpServer}
import spice.http.server.rest.{FileUpload, MultipartRequest, Restful, RestfulResponse}
import spice.net.*

import java.io.File

class ServerSpec extends AnyWordSpec with Matchers {
  object server extends MutableHttpServer

  "TestHttpApplication" should {
    "configure the TestServer" in {
      server.handler.matcher(paths.exact("/test.html")).wrap(new HttpHandler {
        override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = {
          exchange.modify { response =>
            Task(response.withContent(Content.string("test!", ContentType.`text/plain`)))
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
      }.sync()
    }
    "receive NotFound for other.html" in {
      server.handle(HttpExchange(HttpRequest(url = url"http://localhost/other.html"))).map { exchange =>
        exchange.response.status should be(HttpStatus.NotFound)
      }.sync()
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
      }.sync()
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
      }.sync()
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
      }.sync()
    }
    "reverse a String with an informational error" in {
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Get,
        url = url"http://localhost/test/reverse/info"
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.InternalServerError)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val response = json.as[ReverseResponse]
        response.errors should be(List(ValidationError(
          message = "Info Test"
        )))
        response.reversed should be(None)
      }.sync()
    }
    "reverse a String with an warn error" in {
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Get,
        url = url"http://localhost/test/reverse/warn"
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.InternalServerError)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val response = json.as[ReverseResponse]
        response.errors should be(List(ValidationError(
          message = "Warn Test"
        )))
        response.reversed should be(None)
      }.sync()
    }
    "reverse a String with a logged error" in {
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Get,
        url = url"http://localhost/test/reverse/error"
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.InternalServerError)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val response = json.as[ReverseResponse]
        response.errors should be(List(ValidationError(
          message = "Error Test"
        )))
        response.reversed should be(None)
      }.sync()
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
      }.sync()
    }
    "call a Restful endpoint that takes multipart content" in {
      server.handle(HttpExchange(HttpRequest(
        method = HttpMethod.Post,
        url = url"http://localhost/test/file",
        content = Some(FormDataContent
          .withJson("fileName", Str("test.png"))
          .withJson("description", Str("Test Image"))
          .withFile("file", "test.png", new File("test.png"))
        )
      ))).map { exchange =>
        exchange.response.status should be(HttpStatus.OK)
        val json = exchange.response.content.get.asInstanceOf[JsonContent].json
        val fileName = json.as[String]
        fileName should be("test.png")
      }.sync()
    }
  }

  case class ReverseRequest(value: String)

  object ReverseRequest {
    given rw: RW[ReverseRequest] = RW.gen
  }

  case class ReverseResponse(reversed: Option[String], errors: List[ValidationError])

  object ReverseResponse {
    given rw: RW[ReverseResponse] = RW.gen
  }

  object ReverseService extends Restful[ReverseRequest, ReverseResponse] {
    override def apply(exchange: HttpExchange, request: ReverseRequest)
                      (using mdc: MDC): Task[RestfulResponse[ReverseResponse]] = Task {
      val result = request.value match {
        case "info" => throw UserException("Info Test")
        case "warn" => throw UserException("Warn Test", `type` = ExceptionType.Warn)
        case "error" => throw UserException("Error Test", `type` = ExceptionType.Error)
        case s => s.reverse
      }
      RestfulResponse(ReverseResponse(Some(result), Nil), HttpStatus.OK)
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[ReverseResponse] = {
      RestfulResponse(ReverseResponse(None, errors), status)
    }
  }

  object ServerTimeService extends Restful[Unit, Long] {
    override def apply(exchange: HttpExchange, request: Unit)(using mdc: MDC): Task[RestfulResponse[Long]] = {
      Task.pure(RestfulResponse(System.currentTimeMillis(), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Long] = {
      RestfulResponse(0L, status)
    }
  }

  object FileUploadService extends Restful[FileInfo, String] {
    override def apply(exchange: HttpExchange,
                       request: FileInfo)
                      (using mdc: MDC): Task[RestfulResponse[String]] = Task {
      val fileEntry = request.file
      assert(fileEntry.file.length() == 33404)
      ok(request.fileName)
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[String] = RestfulResponse("Failure!", HttpStatus.InternalServerError)
  }

  case class FileInfo(fileName: String, description: String, file: FileUpload)

  object FileInfo {
    given rw: RW[FileInfo] = RW.gen
  }
}