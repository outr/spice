package spec

import rapid._
import cats.effect.testing.scalatest.AsyncIOSpec
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import profig.Profig
import scribe.mdc.MDC
import spice.ValidationError
import spice.http.client.HttpClient
import spice.http._
import spice.http.content.Content
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.dsl._
import spice.http.server.rest.{Restful, RestfulResponse}
import spice.net._

class UndertowServerSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "UndertowServerSpec" should {
    object server extends MutableHttpServer
    def serverPort: Int = server.config.listeners().head.port.getOrElse(0)

    lazy val client = HttpClient.url(url"http://localhost".withPort(serverPort))

    "configure the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler.matcher(paths.exact("/test.txt"))
        .content(Content.string("test!", ContentType.`text/plain`))
      server.handler(
        List(
          path"/test/reverse" / ReverseService,
          path"/test/reverse/:value" / ReverseService,
          path"/test/time" / ServerTimeService,
          path"/test/letters" / LettersOnlyService
        )
      )
      server.handlers.size should be(2)
    }
    "start the server" in {
      server.start().map { _ =>
        server.isRunning should be(true)
      }
    }
    "receive OK for test.txt" in {
      client
        .path(path"/test.txt")
        .send()
        .flatMap { response =>
          response.status should be(HttpStatus.OK)
          response.content.get.asString.map { content =>
            content should be("test!")
          }
        }
    }
    "receive NotFound for test.html" in {
      client
        .path(path"/test.html")
        .send()
        .map { response =>
          response.status should be(HttpStatus.NotFound)
        }
    }
    "reverse a String with the Restful endpoint via POST" in {
      client
        .path(path"/test/reverse")
        .restfulTry[ReverseRequest, ReverseResponse](ReverseRequest("testing"))
        .map { attempt =>
          val response = attempt.getOrElse(fail("Response failure!"))
          response.errors should be(Nil)
          response.reversed should be(Some("gnitset"))
        }
    }
    "reverse a String with the Restful endpoint via GET" in {
      client
        .path(path"/test/reverse")
        .params("value" -> "testing")
        .callTry[ReverseResponse]
        .map { attempt =>
          val response = attempt.getOrElse(fail("Response failure!"))
          response.errors should be(Nil)
          response.reversed should be(Some("gnitset"))
        }
    }
    "reverse a String with the Restful endpoint via GET with path-based arg" in {
      client
        .path(path"/test/reverse/testing")
        .callTry[ReverseResponse]
        .map { attempt =>
          val response = attempt.getOrElse(fail("Response failure!"))
          response.errors should be(Nil)
          response.reversed should be(Some("gnitset"))
        }
    }
    "call a Restful endpoint that takes Unit as the request" in {
      val begin = System.currentTimeMillis()
      client
        .path(path"/test/time")
        .callTry[Long]
        .map(_.getOrElse(throw new RuntimeException("Failure!")))
        .map { time =>
          time should be >= begin
        }
    }
    "call a Restful endpoint that takes a String as the request" in {
      client
        .path(path"/test/letters")
        .restfulTry[String, String]("test1test2test3")
        .map(_.getOrElse(throw new RuntimeException("Failure!")))
        .map { result =>
          result should be("testtesttest")
        }
    }
    "start an adhoc server on any available port and make sure it propagates back" in {
      val testServer = new MutableHttpServer
      testServer.config
        .clearListeners()
        .addListeners(HttpServerListener(port = None))
        .addListeners(HttpServerListener(port = Some(8282)))
      testServer.start().flatMap { _ =>
        testServer.config.listeners().head.port should not be None
        testServer.config.listeners().head.port should not be Some(8282)
        testServer.config.listeners().last.port should be(Some(8282))
        testServer.stop()
      }
    }
    "stop the server" in {
      server.stop().map { _ =>
        server.isRunning should be(false)
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
                      (implicit mdc: MDC): Task[RestfulResponse[ReverseResponse]] = {
      Task.pure(RestfulResponse(ReverseResponse(Some(request.value.reverse), Nil), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[ReverseResponse] = {
      RestfulResponse(ReverseResponse(None, errors), status)
    }
  }

  object LettersOnlyService extends Restful[String, String] {
    override def apply(exchange: HttpExchange, text: String)
                      (implicit mdc: MDC): Task[RestfulResponse[String]] = {
      Task.pure(RestfulResponse[String](text.filter((c: Char) => c.isLetter), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[String] =
      RestfulResponse("failure!", status)
  }

  object ServerTimeService extends Restful[Unit, Long] {
    override def apply(exchange: HttpExchange, request: Unit)
                      (implicit mdc: MDC): Task[RestfulResponse[Long]] = {
      Task.pure(RestfulResponse(System.currentTimeMillis(), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Long] = {
      RestfulResponse(0L, status)
    }
  }
}
