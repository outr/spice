package spec

import fabric.*
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.concurrent.Eventually.*
import org.scalatest.time.{Seconds, Span}
import profig.Profig
import rapid.*
import spice.http.client.HttpClient
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.dsl.*
import spice.http.server.dsl.given
import spice.http.durable.*
import spice.net.*

import scala.concurrent.duration.*

case class ChatEvent(message: String, from: String)
object ChatEvent {
  given rw: RW[ChatEvent] = RW.gen
}

case class ConnectInfo(userId: String, room: String)
object ConnectInfo {
  given rw: RW[ConnectInfo] = RW.gen
}

class DurableSocketSpec extends AnyWordSpec with Matchers {
  val testConfig: DurableSocketConfig = DurableSocketConfig(
    ackBatchDelay = 50.millis,
    ackBatchCount = 3,
    heartbeatInterval = 2.seconds,
    heartbeatTimeout = 6.seconds,
    reconnectStrategy = ReconnectStrategy.none
  )

  val eventLog = new InMemoryEventLog[String, ChatEvent]

  val durableServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
    config = testConfig,
    eventLog = eventLog,
    resolveChannel = (_, info) => Task.pure(info.room)
  )

  object server extends MutableHttpServer

  def serverPort: Int = server.config.listeners().head.port.getOrElse(0)

  def createClient(room: String, userId: String = java.util.UUID.randomUUID().toString): DurableSocketClient[String, ChatEvent, ConnectInfo] = {
    new DurableSocketClient[String, ChatEvent, ConnectInfo](
      createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/ws")).webSocket(),
      config = testConfig,
      outboundLog = eventLog,
      initialChannelId = userId,
      info = ConnectInfo(userId, room),
      clientId = userId
    )
  }

  "DurableSocket over Undertow" should {
    "start the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler(List(
        path"/ws" / durableServer
      ))
      server.start().sync()
      server.isRunning should be(true)
    }

    "connect a client and invoke/respond" in {
      val client = createClient("room1", "user1")

      // Server handles invocations
      durableServer.onSession.attach { session =>
        session.protocol.onInvoke.attach { msg =>
          session.touch()
          msg.tool match {
            case "greet" =>
              val name = msg.args("name").asString
              session.protocol.respond(msg.id, obj("greeting" -> str(s"Hello, $name!"))).start()
            case other =>
              session.protocol.respondError(msg.id, s"Unknown tool: $other").start()
          }
        }
      }

      client.connect().sync()
      client.state() should be(ProtocolState.Active)

      val result = client.invoke("greet", obj("name" -> str("Alice"))).sync()
      result("greeting").asString should be("Hello, Alice!")

      client.close()
    }

    "push typed events from server to client" in {
      val client = createClient("room1", "user2")

      var received: List[(Long, ChatEvent)] = Nil
      client.onEvent.attach { e => received = received :+ e }

      client.connect().sync()

      // Wait for session to be registered
      eventually(timeout(Span(5, Seconds))) {
        durableServer.session("user2") should not be empty
      }

      val session = durableServer.session("user2").get
      val seq = session.protocol.push(ChatEvent("hello", "server")).sync()
      seq should be > 0L

      eventually(timeout(Span(5, Seconds))) {
        received.size should be(1)
        received.head._2 should be(ChatEvent("hello", "server"))
      }

      client.close()
    }

    "broadcast to all clients in a room" in {
      val client1 = createClient("chatroom", "user3")
      val client2 = createClient("chatroom", "user4")

      var received1: List[(Long, ChatEvent)] = Nil
      var received2: List[(Long, ChatEvent)] = Nil
      client1.onEvent.attach { e => received1 = received1 :+ e }
      client2.onEvent.attach { e => received2 = received2 :+ e }

      client1.connect().sync()
      client2.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        durableServer.sessionsByChannel("chatroom").size should be(2)
      }

      // Broadcast a message to the room
      val seq = durableServer.broadcast("chatroom", ChatEvent("announcement", "admin")).sync()

      eventually(timeout(Span(5, Seconds))) {
        received1.size should be(1)
        received2.size should be(1)
        // Both see the same seq
        received1.head._1 should be(seq)
        received2.head._1 should be(seq)
        received1.head._2 should be(ChatEvent("announcement", "admin"))
      }

      client1.close()
      client2.close()
    }

    "reject duplicate messages" in {
      val client = createClient("room1", "user5")
      var eventCount = 0
      client.onEvent.attach { _ => eventCount += 1 }

      client.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        durableServer.session("user5") should not be empty
      }

      val session = durableServer.session("user5").get
      session.protocol.push(ChatEvent("first", "s")).sync()
      session.protocol.push(ChatEvent("second", "s")).sync()

      eventually(timeout(Span(5, Seconds))) {
        eventCount should be(2)
      }

      // Replay seq 1 — should be ignored
      session.protocol.sendLogged(1L, ChatEvent("first", "s"))
      Thread.sleep(200)
      eventCount should be(2)

      client.close()
    }

    "error response fails client Task" in {
      val client = createClient("room1", "user6")

      client.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        durableServer.session("user6") should not be empty
      }

      // onSession handler already registered from "connect a client" test
      val result = client.invoke("unknown_tool", obj()).attempt.sync()
      result.isFailure should be(true)
      result.failed.get.getMessage should include("Unknown tool")

      client.close()
    }

    "replay missed events on reconnect" in {
      val userId = "user7"
      val client = createClient("replay-room", userId)
      var received: List[(Long, ChatEvent)] = Nil
      client.onEvent.attach { e => received = received :+ e }

      client.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        durableServer.session(userId) should not be empty
      }

      val session = durableServer.session(userId).get

      // Push first event
      session.protocol.push(ChatEvent("before", "s")).sync()
      eventually(timeout(Span(5, Seconds))) {
        received.size should be(1)
      }

      // Disconnect the client (but don't close — session persists)
      client.protocol.unbind()

      // Push events while disconnected (to the log)
      session.protocol.push(ChatEvent("during1", "s")).sync()
      session.protocol.push(ChatEvent("during2", "s")).sync()

      // Reconnect — create new WS and resume
      val lastSeq = client.protocol.highestProcessedSeq
      val ws2 = HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/ws")).webSocket()
      ws2.connect().sync()
      client.protocol.bind(ws2)
      client.protocol.sendResume(userId, lastSeq, ConnectInfo(userId, "replay-room"))

      // Wait for replay
      eventually(timeout(Span(5, Seconds))) {
        received.size should be(3)
        received.map(_._2.message) should be(List("before", "during1", "during2"))
      }

      client.close()
    }

    "look up sessions by channel" in {
      durableServer.sessionsByChannel("chatroom").foreach(s => durableServer.removeSession(s.clientId))

      val c1 = createClient("lookup-room", "lookup1")
      val c2 = createClient("lookup-room", "lookup2")
      val c3 = createClient("other-room", "lookup3")

      c1.connect().sync()
      c2.connect().sync()
      c3.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        durableServer.sessionsByChannel("lookup-room").size should be(2)
        durableServer.sessionsByChannel("other-room").size should be(1)
      }

      durableServer.sessionsByChannel("lookup-room").map(_.clientId).toSet should be(Set("lookup1", "lookup2"))
      durableServer.sessionsByChannel("other-room").map(_.clientId) should be(List("lookup3"))

      c1.close()
      c2.close()
      c3.close()
    }

    "expire stale sessions" in {
      // Create a server with very short session timeout
      val shortTimeoutServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
        config = testConfig,
        eventLog = eventLog,
        resolveChannel = (_, info) => Task.pure(info.room),
        sessionTimeout = 100.millis
      )

      server.handler(List(
        path"/ws-expiry" / shortTimeoutServer
      ))

      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/ws-expiry")).webSocket(),
        config = testConfig,
        outboundLog = eventLog,
        initialChannelId = "expiry-user",
        info = ConnectInfo("expiry-user", "expiry-room"),
        clientId = "expiry-user"
      )

      client.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        shortTimeoutServer.session("expiry-user") should not be empty
      }

      // Disconnect the real WebSocket (causes server-side state → Disconnected)
      client.close()

      // Disconnect the real WebSocket
      client.close()

      // Wait for Undertow to detect the closed connection
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        val s = shortTimeoutServer.session("expiry-user")
        s should not be empty
        s.get.isConnected should be(false)
      }

      // Now expire the stale disconnected session
      val expired = shortTimeoutServer.expireStale()
      expired should be(1)
      shortTimeoutServer.session("expiry-user") should be(empty)
    }

    "event log replay returns correct subset" in {
      val testLog = new InMemoryEventLog[String, ChatEvent]
      testLog.append("ch1", ChatEvent("a", "x")).sync()
      testLog.append("ch1", ChatEvent("b", "x")).sync()
      testLog.append("ch1", ChatEvent("c", "x")).sync()

      val replayed = testLog.replay("ch1", 1L).sync()
      replayed.map(_._1) should be(List(2L, 3L))
      replayed.map(_._2) should be(List(ChatEvent("b", "x"), ChatEvent("c", "x")))

      val all = testLog.replay("ch1", 0L).sync()
      all.size should be(3)
    }

    "switch channels and receive events from new channel" in {
      val client = createClient("channelA", "switcher")

      var received: List[(Long, ChatEvent)] = Nil
      client.onEvent.attach { e =>
        received = received :+ e
      }

      client.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        durableServer.session("switcher") should not be empty
      }

      // Push event on channelA
      durableServer.broadcast("channelA", ChatEvent("msg-A", "admin")).sync()
      eventually(timeout(Span(5, Seconds))) {
        received.size should be(1)
        received.head._2.message should be("msg-A")
      }

      // Pre-populate channelB with events
      eventLog.append("channelB", ChatEvent("old-B1", "system")).sync()
      eventLog.append("channelB", ChatEvent("old-B2", "system")).sync()

      // Switch to channelB — client has never seen it, so lastSeq = 0
      client.switch("channelB").sync()

      // Verify the switch replayed channelB's events
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        received.count(e => e._2.message == "old-B1" || e._2.message == "old-B2") should be(2)
      }

      // Push new event on channelB — should arrive
      durableServer.broadcast("channelB", ChatEvent("new-B", "admin")).sync()
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        received.exists(_._2.message == "new-B") should be(true)
      }

      // Switch back to channelA — client remembers last-seen seq for channelA
      client.switch("channelA").sync()

      // Push on channelA — should arrive
      durableServer.broadcast("channelA", ChatEvent("msg-A2", "admin")).sync()
      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        received.exists(_._2.message == "msg-A2") should be(true)
      }

      client.close()
    }

    "stop the server" in {
      server.stop().sync()
      server.isRunning should be(false)
    }
  }
}
