package spec

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.concurrent.Eventually._
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.wordspec.AsyncWordSpec
import spice.http.client.HttpClient
import spice.http.{ConnectionStatus, HttpExchange, WebSocket, WebSocketListener}
import spice.http.server.StaticHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.dsl._
import spice.http.server.handler.{HttpHandler, WebSocketHandler}
import spice.net._

class UndertowWebSocketSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  private var webSocketClient: WebSocket = _
  private var fromServer = List.empty[String]

  "Undertow WebSocket" should {
    def serverPort: Int = server.config.listeners().head.port.getOrElse(0)

    "start the server" in {
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.start().map(_ => succeed)
    }
    "open a WebSocket to the server" in {
      webSocketClient = HttpClient
        .url(url"ws://localhost/webSocket".withPort(serverPort))
        .webSocket()
      webSocketClient.receive.text.attach { text =>
        scribe.info(s"Received $text from server!")
        fromServer = text :: fromServer
      }
      webSocketClient.connect()
        .map { status =>
          status should be(ConnectionStatus.Open)
        }
    }
    "send a message to the server" in {
      webSocketClient.send.text @= "Hello, World!"
      succeed
    }
    "receive a response from the server" in {
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        fromServer should be(List("Hello, World!"))
      }
    }
    "disconnect the client" in {
      webSocketClient.disconnect()
      succeed
    }
    "stop the server" in {
      server.dispose().map(_ => succeed)
    }
  }

  object server extends StaticHttpServer {
    override protected val handler: HttpHandler = filters(
      "webSocket" / EchoWebSocketHandler
    )
  }

  object EchoWebSocketHandler extends WebSocketHandler {
    override def connect(exchange: HttpExchange, listener: WebSocketListener): IO[Unit] = {
      listener.receive.text.attach { text =>
        scribe.info(s"Received: $text from client! Echoing back...")
        listener.send.text @= text
      }

      IO.unit
    }
  }
}