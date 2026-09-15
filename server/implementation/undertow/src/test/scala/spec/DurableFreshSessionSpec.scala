package spec

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
 * A client whose resume lands on a server that no longer holds its session
 * (a restart, a deploy) gets a fresh session numbered from the start again.
 * It must accept that session's events rather than drop them against the
 * previous session's high-water mark, and it must be told the session is new.
 */
class DurableFreshSessionSpec extends AnyWordSpec with Matchers {
  private val config = DurableSocketConfig(
    ackBatchDelay = 50.millis,
    ackBatchCount = 3,
    reconnectStrategy = ReconnectStrategy.exponentialBackoff(baseDelay = 50.millis, maxDelay = 200.millis, maxAttempts = Int.MaxValue)
  )

  /** The server's log; replaced to stand in for a restarted process whose numbering starts over. */
  @volatile private var log = new InMemoryEventLog[String, ChatEvent]
  private val swappableLog = new EventLog[String, ChatEvent] {
    override def append(channelId: String, event: ChatEvent): Task[Long] = log.append(channelId, event)
    override def replay(channelId: String, afterSeq: Long): Task[List[(Long, ChatEvent)]] = log.replay(channelId, afterSeq)
  }

  private val durableServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
    config = config,
    eventLog = swappableLog,
    resolveChannel = (_, info) => Task.pure(info.room)
  )

  object server extends MutableHttpServer

  private def port: Int = server.config.listeners().head.port.getOrElse(0)

  "a resume that gets a fresh session" should {
    "start the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler(List(path"/ws" / durableServer))
      server.start().sync()
      server.isRunning should be(true)
    }

    "deliver the new session's events and report the session as not resumed" in {
      val userId = "fresh-session-user"
      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(port).withPath(path"/ws")).webSocket(),
        config = config,
        outboundLog = new InMemoryEventLog[String, ChatEvent],
        initialChannelId = userId,
        info = ConnectInfo(userId, "fresh-room"),
        clientId = userId
      )
      val received = new CopyOnWriteArrayList[String]()
      val connections = new CopyOnWriteArrayList[Boolean]()
      client.onEvent.attach { case (_, event) => received.add(event.message) }
      client.onConnected.attach(resumed => connections.add(resumed))

      client.connect().sync()
      eventually(timeout(Span(5, Seconds))) { durableServer.session(userId) should not be empty }
      val first = durableServer.session(userId).get
      (1 to 5).foreach(i => first.protocol.push(ChatEvent(s"old-$i", "server")).sync())
      eventually(timeout(Span(5, Seconds))) { received.size should be(5) }

      // The server "restarts": its numbering starts over and it no longer knows the client.
      log = new InMemoryEventLog[String, ChatEvent]
      durableServer.removeSession(userId)

      eventually(timeout(Span(10, Seconds))) {
        val next = durableServer.session(userId)
        next should not be empty
        (next.get.protocol ne first.protocol) should be(true)
        client.state() should be(ProtocolState.Active)
      }
      durableServer.session(userId).get.protocol.push(ChatEvent("new-1", "server")).sync()

      eventually(timeout(Span(10, Seconds))) {
        received.asScala.toList should contain("new-1")
      }
      connections.asScala.toList should be(List(false, false))
      client.close()
    }

    "stop the server" in {
      server.stop().sync()
      server.isRunning should be(false)
    }
  }
}
