package spec

import fabric.*
import fabric.rw.*
import org.scalatest.concurrent.Eventually.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec
import profig.Profig
import rapid.*
import spice.http.client.HttpClient
import spice.http.durable.*
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.dsl.*
import spice.http.server.dsl.given
import spice.net.*

import java.util.concurrent.CopyOnWriteArrayList
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
 * A client sends the moment it hears it is connected; an application attaches its session's handlers once it knows
 * who the session is, which may take a lookup. What the client sent in between reached no one, and its first requests
 * went unanswered. The session now keeps it until the handlers attach.
 */
class DurableReadySpec extends AnyWordSpec with Matchers {
  private val config = DurableSocketConfig(
    ackBatchDelay = 50.millis,
    ackBatchCount = 3,
    reconnectStrategy = ReconnectStrategy.none
  )

  private val durableServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
    config = config,
    eventLog = new InMemoryEventLog[String, ChatEvent],
    resolveChannel = (_, info) => Task.pure(info.room)
  )

  private val ephemerals = new CopyOnWriteArrayList[String]()
  private val events = new CopyOnWriteArrayList[String]()

  // handlers attached after a lookup that takes a while, as an application resolving the session's user does
  durableServer.onSession.attach { session =>
    Task.sleep(500.millis).map { _ =>
      session.protocol.onEphemeral.attach(json => ephemerals.add(json("type").asString))
      session.protocol.onEvent.attach { case (_, event) => events.add(event.message) }
    }.start()
  }

  object server extends MutableHttpServer

  private def port: Int = server.config.listeners().head.port.getOrElse(0)

  "a fresh durable session" should {
    "start the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler(List(path"/ws" / durableServer))
      server.start().sync()
      server.isRunning should be(true)
    }

    "deliver what its client sent before the application's handlers attached" in {
      val userId = "ready-user"
      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(port).withPath(path"/ws")).webSocket(),
        config = config,
        outboundLog = new InMemoryEventLog[String, ChatEvent],
        initialChannelId = userId,
        info = ConnectInfo(userId, "ready-room"),
        clientId = userId
      )
      client.connect().sync()
      client.sendEphemeral(obj("type" -> str("history")))
      client.sendEphemeral(obj("type" -> str("stores")))
      client.push(ChatEvent("hello", userId)).sync()

      eventually(timeout(Span(5, Seconds))) {
        ephemerals.asScala.toList should be(List("history", "stores"))
        events.asScala.toList should be(List("hello"))
      }
      client.close()
    }

    "stop the server" in {
      server.stop().sync()
      server.isRunning should be(false)
    }
  }
}
