package spec

import fabric.io.JsonParser
import fabric.rw.*
import org.scalatest.concurrent.Eventually.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec
import profig.Profig
import rapid.*
import spice.api.WsClient
import spice.api.server.WsServer
import spice.http.client.HttpClient
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.net.*

import scala.collection.mutable.ListBuffer

trait RoutedEvents {
  def userMessage(sessionId: String, text: String): Task[Unit]
}

object RoutedWsApiSpec {
  val session1Messages: ListBuffer[String] = ListBuffer.empty
  val session2Messages: ListBuffer[String] = ListBuffer.empty

  class Session1Handler extends RoutedEvents {
    override def userMessage(sessionId: String, text: String): Task[Unit] = Task {
      session1Messages.synchronized { session1Messages += text }
    }
  }

  class Session2Handler extends RoutedEvents {
    override def userMessage(sessionId: String, text: String): Task[Unit] = Task {
      session2Messages.synchronized { session2Messages += text }
    }
  }
}

class RoutedWsApiSpec extends AnyWordSpec with Matchers {
  import RoutedWsApiSpec.*

  "RoutedWsApiSpec" should {
    val server = new MutableHttpServer {}
    def serverPort: Int = server.config.listeners().head.port.getOrElse(0)

    def fetchToken(routingKey: String): String = {
      val response = HttpClient
        .url(url"http://localhost".withPort(serverPort).withPath(path"/ws/events/token").withParam("routingKey", routingKey))
        .get
        .send()
        .sync()
      val json = JsonParser(response.content.get.asString.sync())
      json("token").asString
    }

    "configure and start server with routed WS" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      val events = WsServer.deriveRouted[RoutedEvents](server, path"/ws/events")
      server.start().sync()
      server.isRunning should be(true)

      // Test 1: Token generation
      val token1 = fetchToken("session1")
      token1 should not be empty

      // Test 2: Token-based WS connect
      val ws1 = WsClient.connect[RoutedEvents](
        url"ws://localhost".withPort(serverPort).withPath(path"/ws/events").withParam("token", token1),
        new Session1Handler
      ).sync()
      Thread.sleep(500)
      ws1.status() should be(spice.http.ConnectionStatus.Open)

      // Test 3: Routed delivery — session1 receives its messages
      events.userMessage("session1", "hello").sync()
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        session1Messages.toList should be(List("hello"))
      }

      // Test 4: Multiple sessions — events are isolated
      val token2 = fetchToken("session2")
      val ws2 = WsClient.connect[RoutedEvents](
        url"ws://localhost".withPort(serverPort).withPath(path"/ws/events").withParam("token", token2),
        new Session2Handler
      ).sync()
      Thread.sleep(500)

      events.userMessage("session2", "world").sync()
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        session2Messages.toList should be(List("world"))
      }
      // session1 should NOT have received session2's message
      session1Messages.toList should be(List("hello"))

      // Test 5: Pending message buffering — send before client connects
      events.userMessage("session3", "buffered").sync()
      Thread.sleep(200)

      val session3Messages = ListBuffer.empty[String]
      val token3 = fetchToken("session3")
      val ws3 = WsClient.connect[RoutedEvents](
        url"ws://localhost".withPort(serverPort).withPath(path"/ws/events").withParam("token", token3),
        new RoutedEvents {
          override def userMessage(sessionId: String, text: String): Task[Unit] = Task {
            session3Messages.synchronized { session3Messages += text }
          }
        }
      ).sync()
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        session3Messages.toList should be(List("buffered"))
      }

      // Test 6: Invalid token — connection doesn't receive messages
      val session4Messages = ListBuffer.empty[String]
      val ws4 = WsClient.connect[RoutedEvents](
        url"ws://localhost".withPort(serverPort).withPath(path"/ws/events").withParam("token", "invalid-token"),
        new RoutedEvents {
          override def userMessage(sessionId: String, text: String): Task[Unit] = Task {
            session4Messages.synchronized { session4Messages += text }
          }
        }
      ).sync()
      Thread.sleep(500)
      events.userMessage("session4", "should-not-arrive").sync()
      Thread.sleep(500)
      session4Messages.toList should be(empty)

      // Test 7: Single-use token — reusing a token should fail to route
      val session5Messages = ListBuffer.empty[String]
      val token5 = fetchToken("session5")
      // Use the token once
      val ws5a = WsClient.connect[RoutedEvents](
        url"ws://localhost".withPort(serverPort).withPath(path"/ws/events").withParam("token", token5),
        new RoutedEvents {
          override def userMessage(sessionId: String, text: String): Task[Unit] = Task {
            session5Messages.synchronized { session5Messages += text }
          }
        }
      ).sync()
      Thread.sleep(500)
      // Reuse same token — should not be routed
      val session5bMessages = ListBuffer.empty[String]
      val ws5b = WsClient.connect[RoutedEvents](
        url"ws://localhost".withPort(serverPort).withPath(path"/ws/events").withParam("token", token5),
        new RoutedEvents {
          override def userMessage(sessionId: String, text: String): Task[Unit] = Task {
            session5bMessages.synchronized { session5bMessages += text }
          }
        }
      ).sync()
      Thread.sleep(500)
      events.userMessage("session5", "only-first").sync()
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        session5Messages.toList should be(List("only-first"))
      }
      session5bMessages.toList should be(empty)

      // Test 8: Disconnect cleanup
      ws1.disconnect()
      Thread.sleep(500)
      val session1After = ListBuffer.empty[String]
      events.userMessage("session1", "after-disconnect").sync()
      Thread.sleep(500)
      // Original session1Messages should not get new message
      session1Messages.toList should be(List("hello"))

      server.stop().sync()
    }
  }
}
