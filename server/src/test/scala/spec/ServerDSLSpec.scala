package spec

import rapid.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.{AnyWordSpec, AsyncWordSpec}
import spice.ValidationError
import spice.http.content.Content
import spice.http.server.{DefaultErrorHandler, HttpServer, MutableHttpServer, StaticHttpServer}
import spice.http.{HttpExchange, HttpMethod, HttpRequest, HttpStatus}
import spice.http.server.dsl.*
import spice.http.server.dsl.given
import spice.http.server.handler.{HttpHandler, LifecycleHandler}
import spice.http.server.rest.{Restful, RestfulResponse}
import spice.net.*
import fabric.rw.*

class ServerDSLSpec extends AnyWordSpec with Matchers {
  private lazy val text = "Hello, World!".withContentType(ContentType.`text/plain`)
  private lazy val textPost = "Hello, Post!".withContentType(ContentType.`text/plain`)
  private lazy val html = """<html>
    <head>
      <title>Hello, World!</title>
    </head>
    <body>
      <h1>Hello, World!</h1>
    </body>
  </html>""".withContentType(ContentType.`text/html`)

  "Server DSL" when {
    "creating a simple handler with MutableHttpServer" should {
      "properly accept a request for /hello/world.txt" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/world.txt")
        mutableServer.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(text))
          response.status should be(HttpStatus.OK)
        }.sync()
      }
      "properly accept a request for /hello/world.html" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/world.html")
        mutableServer.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(html))
          response.status should be(HttpStatus.OK)
        }.sync()
      }
      "properly return a 404 for /hello/other.html" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/other.html")
        mutableServer.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.status should be(HttpStatus.NotFound)
        }.sync()
      }
      "properly return a 404 for a POST" in {
        val request = HttpRequest(
          source = ip"127.0.0.1",
          url = url"http://www.example.com/hello/world.html",
          method = HttpMethod.Post
        )
        mutableServer.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.status should be(HttpStatus.NotFound)
        }.sync()
      }
      "reject a request from a different origin IP" in {
        val request = HttpRequest(source = ip"127.0.0.2", url = url"http://www.example.com/hello/world.html")
        mutableServer.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(DefaultErrorHandler.html(HttpStatus.NotFound)))
          response.status should be(HttpStatus.NotFound)
        }.sync()
      }
    }
    "creating a HttpServer directly" should {
      "match a simple GET" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/world.txt")
        server.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(text))
          response.status should be(HttpStatus.OK)
        }.sync()
      }
      "match a simple POST" in {
        val request = HttpRequest(
          source = ip"127.0.0.1",
          url = url"http://www.example.com/hello/world.txt",
          method = HttpMethod.Post
        )
        server.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(textPost))
          response.status should be(HttpStatus.OK)
        }.sync()
      }
      "match not-found" in {
        val request = HttpRequest(
          source = ip"127.0.0.1",
          url = url"http://www.example.com/bad-url"
        )
        server.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(LifecycleHandler.DefaultNotFound))
          response.status should be(HttpStatus.NotFound)
        }.sync()
      }
    }
    "specific testing with filters" should {
      var triggered = List.empty[String]

      val r1 = Restful[String, String]({ request =>
        triggered = triggered ::: List("r1")
        Task(request.reverse)
      }, Some(path"/r1"))
      val r2 = Restful[String, String]({ request =>
        triggered = triggered ::: List("r2")
        Task(request.capitalize)
      }, Some(path"/r2"))
      val f: ConnectionFilter = filters(
        r1,
        r2
      )

      "properly process the first path" in {
        f
          .handle(HttpExchange(HttpRequest(
            url = url"http://localhost:8080/r1",
            content = Some(Content.string("testing", ContentType.`text/plain`))
          )))
          .flatMap { exchange =>
            exchange.response.content.get.asString.map { s =>
              s should be("\"gnitset\"")
            }
          }.sync()
      }
      "properly fall through to the second path" in {
        f
          .handle(HttpExchange(HttpRequest(
            url = url"http://localhost:8080/r2",
            content = Some(Content.string("testing", ContentType.`text/plain`))
          )))
          .flatMap { exchange =>
            exchange.response.content.get.asString.map { s =>
              s should be("\"Testing\"")
            }
          }.sync()
      }
    }
  }

  object mutableServer extends MutableHttpServer {
    handler(
      allow(ip"127.0.0.1", ip"192.168.1.1") / HttpMethod.Get / "hello" / List(
        "world.txt" / text,
        "world.html" / html
      )
    )
  }

  object server extends StaticHttpServer {
    override protected val handler: HttpHandler = {
      allow(ip"127.0.0.1", ip"192.168.1.1") / List(
        HttpMethod.Get / "hello" / List(
          "world.txt" / text,
          "world.html" / html
        ),
        HttpMethod.Post / "hello" / "world.txt" / textPost
      )
    }
  }
}