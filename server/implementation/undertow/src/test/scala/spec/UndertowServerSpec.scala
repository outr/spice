package spec

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import profig.Profig
import spice.ValidationError
import spice.http.client.HttpClient
import spice.http._
import spice.http.content.Content
import spice.http.server.MutableHttpServer
import spice.http.server.dsl._
import spice.http.server.rest.{Restful, RestfulResponse}
import spice.net._

class UndertowServerSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "UndertowServerSpec" should {
    object server extends MutableHttpServer
    val client = HttpClient.url(url"http://localhost:8080")

    "configure the server" in {
      Profig.initConfiguration()
      server.handler.matcher(paths.exact("/test.txt")).wrap(_.modify { response =>
        IO(response.withContent(Content.string("test!", ContentType.`text/plain`)))
      })
      server.handler(
        filters(
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
        .send(retries = 0)
        .map { response =>
          response.status should be(HttpStatus.OK)
          val content = response.content.get.asString
          content should be("test!")
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
    override def apply(exchange: HttpExchange, request: ReverseRequest): IO[RestfulResponse[ReverseResponse]] = {
      IO.pure(RestfulResponse(ReverseResponse(Some(request.value.reverse), Nil), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[ReverseResponse] = {
      RestfulResponse(ReverseResponse(None, errors), status)
    }
  }

  object LettersOnlyService extends Restful[String, String] {
    override def apply(exchange: HttpExchange, text: String): IO[RestfulResponse[String]] = {
      IO.pure(RestfulResponse[String](text.filter((c: Char) => c.isLetter), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[String] =
      RestfulResponse("failure!", status)
  }

  object ServerTimeService extends Restful[Unit, Long] {
    override def apply(exchange: HttpExchange, request: Unit): IO[RestfulResponse[Long]] = {
      IO.pure(RestfulResponse(System.currentTimeMillis(), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Long] = {
      RestfulResponse(0L, status)
    }
  }
}
