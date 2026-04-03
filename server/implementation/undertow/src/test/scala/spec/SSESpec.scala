package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.*
import spice.http.client.HttpClient
import spice.http.content.SSEContent
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.{HttpExchange, paths}
import spice.net.*

class SSESpec extends AnyWordSpec with Matchers {
  "SSE" should {
    object server extends MutableHttpServer

    "configure and start the server" in {
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler.matcher(paths.exact("/sse")).handle { exchange =>
        val events = rapid.Stream.emits(List(
          "data: event1\n\n",
          "data: event2\n\n",
          "data: event3\n\n",
          "data: [DONE]\n\n"
        ))
        exchange.modify { response =>
          Task(response.withContent(SSEContent(events)))
        }.map(_.finish())
      }
      server.start().sync()
      server.isRunning should be(true)
    }

    "stream events from server to client" in {
      val port = server.config.listeners().head.port.getOrElse(0)
      val result = HttpClient
        .url(url"http://localhost".withPort(port))
        .path(path"/sse")
        .get
        .streamLines()
        .flatMap(_.toList)
        .sync()

      val dataLines = result.filter(_.startsWith("data: "))
      dataLines should have size 4
      dataLines.head should be("data: event1")
      dataLines(1) should be("data: event2")
      dataLines(2) should be("data: event3")
      dataLines(3) should be("data: [DONE]")
    }

    "stop the server" in {
      server.dispose().sync()
    }
  }
}
