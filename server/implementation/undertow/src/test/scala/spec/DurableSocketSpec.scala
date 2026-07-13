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

case class EchoRequest(text: String)
object EchoRequest {
  given rw: RW[EchoRequest] = RW.gen
}

case class EchoResponse(text: String)
object EchoResponse {
  given rw: RW[EchoResponse] = RW.gen
}

class DurableSocketSpec extends AnyWordSpec with Matchers {
  val testConfig: DurableSocketConfig = DurableSocketConfig(
    ackBatchDelay = 50.millis,
    ackBatchCount = 3,
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

    "connect a client and exchange events" in {
      val client = createClient("room1", "user1")

      var serverReceived: List[(Long, ChatEvent)] = Nil

      durableServer.onSession.attach { session =>
        session.protocol.onEvent.attach { case (seq, event) =>
          serverReceived = serverReceived :+ (seq, event)
          session.protocol.push(ChatEvent(s"echo: ${event.message}", "server")).start()
        }
      }

      var clientReceived: List[(Long, ChatEvent)] = Nil
      client.onEvent.attach { e => clientReceived = clientReceived :+ e }

      client.connect().sync()
      client.state() should be(ProtocolState.Active)

      client.push(ChatEvent("hello", "user1")).sync()

      eventually(timeout(Span(5, Seconds))) {
        serverReceived.size should be(1)
        serverReceived.head._2.message should be("hello")
      }

      eventually(timeout(Span(5, Seconds))) {
        clientReceived.size should be(1)
        clientReceived.head._2.message should be("echo: hello")
      }

      client.close()
    }

    "push typed events from server to client" in {
      val client = createClient("room1", "user2")

      var received: List[(Long, ChatEvent)] = Nil
      client.onEvent.attach { e => received = received :+ e }

      client.connect().sync()

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

      val seq = durableServer.broadcast("chatroom", ChatEvent("announcement", "admin")).sync()

      eventually(timeout(Span(5, Seconds))) {
        received1.size should be(1)
        received2.size should be(1)
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

    "deliver ephemeral messages outside the durable protocol" in {
      val client = createClient("room1", "user-eph")

      var ephemeral: List[Json] = Nil
      client.onEphemeral.attach { json => ephemeral = ephemeral :+ json }

      client.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        durableServer.session("user-eph") should not be empty
      }

      val session = durableServer.session("user-eph").get
      session.protocol.sendEphemeral(obj(
        "kind" -> str("notification"),
        "text" -> str("You have a new message")
      ))

      eventually(timeout(Span(5, Seconds))) {
        ephemeral.size should be(1)
        ephemeral.head("kind").asString should be("notification")
        ephemeral.head("text").asString should be("You have a new message")
      }

      // Ephemeral messages should NOT appear in the event log
      val logged = eventLog.replay("room1", 0L).sync()
      logged.exists(_._2 == ChatEvent("You have a new message", "notification")) should be(false)

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

    "reject unauthorized channel switch" in {
      val restrictedLog = new InMemoryEventLog[String, ChatEvent]
      val restrictedServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
        config = testConfig,
        eventLog = restrictedLog,
        resolveChannel = (_, info) => Task.pure(info.room),
        authorizeSwitch = (session, targetChannel) => {
          // Only allow switching to rooms that start with the user's ID
          if (targetChannel.startsWith(session.info.userId)) Task.unit
          else Task.error(RuntimeException(s"Access denied to channel: $targetChannel"))
        }
      )

      server.handler(List(
        path"/ws-restricted" / restrictedServer
      ))

      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/ws-restricted")).webSocket(),
        config = testConfig,
        outboundLog = restrictedLog,
        initialChannelId = "authuser",
        info = ConnectInfo("authuser", "authuser-room"),
        clientId = "authuser"
      )

      var errors: List[ErrorMessage] = Nil
      client.protocol.onError.attach { e => errors = errors :+ e }

      client.connect().sync()

      eventually(timeout(Span(5, Seconds))) {
        restrictedServer.session("authuser") should not be empty
      }

      // Allowed switch — channel starts with userId
      client.switch("authuser-other").sync()
      eventually(timeout(Span(5, Seconds))) {
        client.protocol.channelId should be("authuser-other")
      }

      // Unauthorized switch — channel doesn't start with userId
      // Send as ephemeral with "type" field — DurableSocket routes it to handleHandshakeMessage
      // which triggers authorizeSwitch on the server
      client.protocol.sendEphemeral(obj(
        "type" -> str("switch"),
        "channelId" -> str("secret-room"),
        "lastSeq" -> num(0L)
      ))

      eventually(timeout(Span(5, Seconds))) {
        errors.exists(_.code == "access_denied") should be(true)
      }

      // Channel should still be the last authorized one
      client.protocol.channelId should be("authuser-other")

      client.close()
    }

    "expire stale sessions" in {
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

      client.close()

      eventually(timeout(Span(5, Seconds))) {
        Thread.sleep(100)
        val s = shortTimeoutServer.session("expiry-user")
        s should not be empty
        s.get.isConnected should be(false)
      }

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

    "answer a typed request/response RPC" in {
      val rpcLog = new InMemoryEventLog[String, ChatEvent]
      val rpcServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
        config = testConfig,
        eventLog = rpcLog,
        resolveChannel = (_, info) => Task.pure(info.room),
        onRequest = (info, data) => {
          val req = data.as[EchoRequest]
          Task.pure(EchoResponse(s"${info.userId}:${req.text}").json)
        }
      )
      server.handler(List(path"/ws-rpc" / rpcServer))

      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/ws-rpc")).webSocket(),
        config = testConfig,
        outboundLog = rpcLog,
        initialChannelId = "rpcuser",
        info = ConnectInfo("rpcuser", "rpcroom"),
        clientId = "rpcuser"
      )

      client.connect().sync()
      client.state() should be(ProtocolState.Active)

      val response = client.ask[EchoRequest, EchoResponse](EchoRequest("hello")).sync()
      response.text should be("rpcuser:hello")

      client.close()
    }

    "surface an RPC handler error as a coded RpcException" in {
      val errLog = new InMemoryEventLog[String, ChatEvent]
      val errServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
        config = testConfig,
        eventLog = errLog,
        resolveChannel = (_, info) => Task.pure(info.room),
        onRequest = (_, _) => Task.error(new RpcException("bad_input", "rejected"))
      )
      server.handler(List(path"/ws-rpc-err" / errServer))

      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/ws-rpc-err")).webSocket(),
        config = testConfig,
        outboundLog = errLog,
        initialChannelId = "erruser",
        info = ConnectInfo("erruser", "errroom"),
        clientId = "erruser"
      )

      client.connect().sync()

      val result = client.ask[EchoRequest, EchoResponse](EchoRequest("x")).attempt.sync()
      result.isFailure should be(true)
      result.failed.get match {
        case e: RpcException => e.code should be("bad_input")
        case other          => fail(s"Expected RpcException, got $other")
      }

      client.close()
    }

    // `reconnect()` exists for the rolling-deploy case: the instance holding a
    // socket is being retired and wants the client to land on its replacement.
    // `close()` cannot serve that role — it parks the protocol in `Closed`,
    // which permanently disables the reconnect path.
    "reconnect() re-dials the socket without closing the protocol" in {
      // The shared testConfig uses ReconnectStrategy.none; an intentional
      // reconnect needs a strategy that actually yields a delay.
      val reconnectConfig = testConfig.copy(
        reconnectStrategy = ReconnectStrategy.exponentialBackoff(
          baseDelay = 50.millis, maxDelay = 200.millis, maxAttempts = Int.MaxValue
        )
      )
      val userId = "reconnect-user"
      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/ws")).webSocket(),
        config = reconnectConfig,
        outboundLog = eventLog,
        initialChannelId = userId,
        info = ConnectInfo(userId, "reconnect-room"),
        clientId = userId
      )

      var received: List[ChatEvent] = Nil
      client.onEvent.attach { case (_, event) => received = received :+ event }

      client.connect().sync()
      client.state() should be(ProtocolState.Active)

      client.reconnect()

      // The distinguishing property: NOT terminal. `close()` would leave this
      // `Closed` forever.
      client.state() should not be ProtocolState.Closed

      // It re-dials on its own and comes back up.
      eventually(timeout(Span(10, Seconds))) {
        client.state() should be(ProtocolState.Active)
      }

      // And the reconnected socket is genuinely live — a push still round-trips
      // through the server's echo handler.
      client.push(ChatEvent("after-reconnect", userId)).sync()
      eventually(timeout(Span(10, Seconds))) {
        received.map(_.message) should contain("echo: after-reconnect")
      }

      client.close()
      client.state() should be(ProtocolState.Closed)
    }

    "reconnect() is a no-op once closed" in {
      val userId = "closed-user"
      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/ws")).webSocket(),
        config = testConfig,
        outboundLog = eventLog,
        initialChannelId = userId,
        info = ConnectInfo(userId, "closed-room"),
        clientId = userId
      )
      client.connect().sync()
      client.close()
      client.state() should be(ProtocolState.Closed)

      // A caller that closed deliberately must stay closed — reconnect() must
      // not resurrect a socket the owner already gave up on.
      client.reconnect()
      client.state() should be(ProtocolState.Closed)
    }

    "stop the server" in {
      server.stop().sync()
      server.isRunning should be(false)
    }
  }
}
