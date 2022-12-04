package spec

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import spice.http.server.{DefaultErrorHandler, HttpServer, MutableHttpServer, StaticHttpServer}
import spice.http.{HttpExchange, HttpMethod, HttpRequest, HttpStatus}
import spice.http.server.dsl._
import spice.http.server.handler.HttpHandler
import spice.net._

class ServerDSLSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
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
        }
      }
      "properly accept a request for /hello/world.html" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/world.html")
        mutableServer.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(html))
          response.status should be(HttpStatus.OK)
        }
      }
      "properly return a 404 for /hello/other.html" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/other.html")
        mutableServer.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.status should be(HttpStatus.NotFound)
        }
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
        }
      }
      "reject a request from a different origin IP" in {
        val request = HttpRequest(source = ip"127.0.0.2", url = url"http://www.example.com/hello/world.html")
        mutableServer.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(DefaultErrorHandler.html(HttpStatus.NotFound)))
          response.status should be(HttpStatus.NotFound)
        }
      }
    }
    "creating a HttpServer directly" should {
      "match a simple GET" in {
        val request = HttpRequest(source = ip"127.0.0.1", url = url"http://www.example.com/hello/world.txt")
        server.handle(HttpExchange(request)).map { exchange =>
          val response = exchange.response
          response.content should be(Some(text))
          response.status should be(HttpStatus.OK)
        }
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
        }
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