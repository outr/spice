package spec

import io.circe.generic.auto._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import profig.JsonUtil
import spice.net._
import spice.http.{HttpRequest, HttpResponse}
import spice.server.handler.HttpHandler
import spice.server.{Server, ServerImplementation}

import scala.concurrent.Future
import scala.language.implicitConversions

class ServerSpec extends AsyncWordSpec with Matchers {
  implicit val serverImplementation: ServerImplementation = new ServerImplementation {}

  trait HttpChain extends HttpHandler {
    override def apply(request: HttpRequest, response: HttpResponse): Future[HttpResponse] = chain(request, response).response

    def chain(request: HttpRequest, response: HttpResponse): Future[HttpResult]

    def /(next: HttpChain): HttpChain = HttpChain.chained(this, next)

    def ||(other: HttpChain): HttpChain = HttpChain.or(this, other)
  }

  object HttpChain {
    def apply(handler: HttpHandler): HttpChain = new HttpChain {
      override def chain(request: HttpRequest, response: HttpResponse): Future[HttpResult] = {
        handler(request, response).map(HttpResult.Continue)
      }
    }
    def chained(first: HttpChain, second: HttpChain): HttpChain = new HttpChain {
      override def chain(request: HttpRequest, response: HttpResponse): Future[HttpResult] = {
        first.chain(request, response).flatMap {
          case HttpResult.Continue(r) => second.chain(request, r)
          case r: HttpResult.Stop => Future.successful(r)
        }
      }
    }
    def or(first: HttpChain, second: HttpChain): HttpChain = new HttpChain {
      override def chain(request: HttpRequest, response: HttpResponse): Future[HttpResult] = {
        first.chain(request, response).flatMap {
          case r: HttpResult.Continue => Future.successful(r)
          case HttpResult.Stop(r) => second.chain(request, r)
        }
      }
    }
    def filter(f: HttpRequest => Future[Boolean]): HttpChain = new HttpChain {
      override def chain(request: HttpRequest, response: HttpResponse): Future[HttpResult] = {
        f(request).map {
          case true => HttpResult.Continue(response)
          case false => HttpResult.Stop(response)
        }
      }
    }
  }

  sealed trait HttpResult {
    def response: HttpResponse
  }

  object HttpResult {
    case class Continue(response: HttpResponse) extends HttpResult
    case class Stop(response: HttpResponse) extends HttpResult
  }

  implicit def path2Chain(path: Path): HttpChain = HttpChain.filter(r => Future.successful(r.url.path == path))
  implicit def handler2Chain(handler: HttpHandler): HttpChain = HttpChain(handler)

  val handler: HttpHandler = (path"/test.html" / Content.string("test!", ContentType.`text/plain`)) ||
    (path"/test/reverse" / ReverseService) ||
    (path"/test/reverse/:value" / ReverseService) ||
    (path"/test/time" / ServerTimeService)

  object server extends Server(handler)

  "TestHttpApplication" should {
    "configure Restful endpoint" in {
      server.handler(
        filters(
          path"/test/reverse" / ReverseService,
          path"/test/reverse/:value" / ReverseService,
          path"/test/time" / ServerTimeService
        )
      )
      server.handlers.size should be(2)
    }
    "receive OK for test.html" in {
      server.handle(HttpConnection(server, HttpRequest(url = URL("http://localhost/test.html")))).map { connection =>
        connection.response.status should equal(HttpStatus.OK)
      }
    }
    "receive NotFound for other.html" in {
      server.handle(HttpConnection(server, HttpRequest(url = URL("http://localhost/other.html")))).map { connection =>
        connection.response.status should equal(HttpStatus.NotFound)
      }
    }
    "reverse a String with the Restful endpoint via POST" in {
      val content = Content.string(JsonUtil.toJsonString(ReverseRequest("Testing")), ContentType.`application/json`)
      server.handle(HttpConnection(server, HttpRequest(
        method = HttpMethod.Post,
        url = URL("http://localhost/test/reverse"),
        content = Some(content)
      ))).map { connection =>
        connection.response.status should equal(HttpStatus.OK)
        connection.response.content shouldNot equal(None)
        val jsonString = connection.response.content.get.asInstanceOf[StringContent].value
        val response = JsonUtil.fromJsonString[ReverseResponse](jsonString)
        response.errors should be(Nil)
        response.reversed should be(Some("gnitseT"))
      }
    }
    "reverse a String with the Restful endpoint via GET" in {
      server.handle(HttpConnection(server, HttpRequest(
        method = HttpMethod.Get,
        url = URL("http://localhost/test/reverse?value=Testing")
      ))).map { connection =>
        connection.response.status should equal(HttpStatus.OK)
        connection.response.content shouldNot equal(None)
        val jsonString = connection.response.content.get.asInstanceOf[StringContent].value
        val response = JsonUtil.fromJsonString[ReverseResponse](jsonString)
        response.errors should be(Nil)
        response.reversed should be(Some("gnitseT"))
      }
    }
    "reverse a String with the Restful endpoint via GET with path-based arg" in {
      server.handle(HttpConnection(server, HttpRequest(
        method = HttpMethod.Get,
        url = URL("http://localhost/test/reverse/Testing")
      ))).map { connection =>
        connection.response.status should equal(HttpStatus.OK)
        connection.response.content shouldNot equal(None)
        val jsonString = connection.response.content.get.asInstanceOf[StringContent].value
        val response = JsonUtil.fromJsonString[ReverseResponse](jsonString)
        response.errors should be(Nil)
        response.reversed should be(Some("gnitseT"))
      }
    }
    "call a Restful endpoint that takes Unit as the request" in {
      val begin = System.currentTimeMillis()
      server.handle(HttpConnection(server, HttpRequest(
        method = HttpMethod.Get,
        url = URL("http://localhost/test/time")
      ))).map { connection =>
        connection.response.status should equal(HttpStatus.OK)
        connection.response.content shouldNot equal(None)
        val jsonString = connection.response.content.get.asInstanceOf[StringContent].value
        val response = JsonUtil.fromJsonString[Long](jsonString)
        response should be >= begin
      }
    }
  }

  case class ReverseRequest(value: String)

  case class ReverseResponse(reversed: Option[String], errors: List[ValidationError])

  trait Restful[Input, Output] extends HttpHandler {
    override def apply(request: HttpRequest, response: HttpResponse): Future[HttpResponse] = ???

    def apply(request: HttpRequest, response: HttpResponse, input: Input): Future[Output]   // TODO: return Future[RestfulResponse[Output]]
  }

  object ReverseService extends Restful[ReverseRequest, ReverseResponse] {
    override def apply(request: HttpRequest,
                       response: HttpResponse,
                       input: ReverseRequest): Future[ReverseResponse] = {
      Future.successful(ReverseResponse(Some(input.value.reverse), Nil))
    }
  }

//  object ReverseService extends Restful[ReverseRequest, ReverseResponse] {
//    override def apply(connection: HttpConnection, request: ReverseRequest): Future[RestfulResponse[ReverseResponse]] = {
//      Future.successful(RestfulResponse(ReverseResponse(Some(request.value.reverse), Nil), HttpStatus.OK))
//    }
//
//    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[ReverseResponse] = {
//      RestfulResponse(ReverseResponse(None, errors), status)
//    }
//  }

  object ServerTimeService extends Restful[Unit, Long] {
    override def apply(connection: HttpConnection, request: Unit): Future[RestfulResponse[Long]] = {
      Future.successful(RestfulResponse(System.currentTimeMillis(), HttpStatus.OK))
    }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Long] = {
      RestfulResponse(0L, status)
    }
  }
}