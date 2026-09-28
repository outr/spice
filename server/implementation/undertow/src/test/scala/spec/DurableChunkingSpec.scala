package spice.http.durable

import fabric.*
import fabric.rw.*
import org.scalatest.concurrent.Eventually.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec
import profig.Profig
import rapid.*
import spec.{ChatEvent, ConnectInfo, EchoRequest, EchoResponse}
import spice.http.client.HttpClient
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.dsl.*
import spice.http.server.dsl.given
import spice.net.*

import scala.concurrent.duration.*

/**
 * A message longer than a frame crosses a durable socket as chunks, both ways, and arrives whole; a message the
 * receiver will not take, or cannot read, is answered — a request by its id, anything else with an error frame —
 * rather than dropped; and one over the sender's own limit fails where it is sent.
 */
class DurableChunkingSpec extends AnyWordSpec with Matchers {
  private val base = DurableSocketConfig(ackBatchDelay = 50.millis, ackBatchCount = 3, reconnectStrategy = ReconnectStrategy.none)
  private val chunked = base.copy(maxFrameChars = 1000)

  object server extends MutableHttpServer
  private def port: Int = server.config.listeners().head.port.getOrElse(0)

  private def client(path: URLPath, config: DurableSocketConfig, log: EventLog[String, ChatEvent], id: String) =
    new DurableSocketClient[String, ChatEvent, ConnectInfo](
      createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(port).withPath(path)).webSocket(),
      config = config, outboundLog = log, initialChannelId = id, info = ConnectInfo(id, "room"), clientId = id)

  /** Longer than many frames, with a surrogate pair where a naive cut would split it. */
  private val long: String = ("é" * 999 + "😀") * 60

  "A durable socket" should {
    "start the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.start().sync()
      server.isRunning should be(true)
    }

    "carry a message longer than a frame as chunks, both ways, arriving whole" in {
      val log = new InMemoryEventLog[String, ChatEvent]
      val durable = new DurableSocketServer[String, ChatEvent, ConnectInfo](config = chunked, eventLog = log,
        resolveChannel = (_, info) => Task.pure(info.room))
      server.handler(List(path"/ws-chunks" / durable))
      @volatile var serverSocket: Option[DurableSocket[String, ChatEvent, ConnectInfo]] = None
      @volatile var atServer: Option[String] = None
      durable.onSession.attach { session =>
        serverSocket = Some(session.protocol)
        session.protocol.onEvent.attach { case (_, event) =>
          atServer = Some(event.message)
          session.protocol.push(ChatEvent(s"echo:${event.message}", "server")).start()
        }
      }
      val c = client(path"/ws-chunks", chunked, log, "chunker")
      @volatile var atClient: Option[String] = None
      c.onEvent.attach { case (_, event) => atClient = Some(event.message) }
      c.connect().sync()
      c.push(ChatEvent(long, "chunker")).sync()
      eventually(timeout(Span(10, Seconds))) {
        atServer shouldBe Some(long)
        atClient shouldBe Some(s"echo:$long")
      }
      c.protocol.chunkedMessagesSent should be > 0L
      serverSocket.map(_.chunkedMessagesSent).getOrElse(0L) should be > 0L
      c.close()
    }

    "answer a request longer than the receiver takes with a response-error for it" in {
      val log = new InMemoryEventLog[String, ChatEvent]
      val durable = new DurableSocketServer[String, ChatEvent, ConnectInfo](config = base.copy(maxMessageChars = 10000), eventLog = log,
        resolveChannel = (_, info) => Task.pure(info.room),
        onRequest = (_, data) => Task.pure(EchoResponse(data.as[EchoRequest].text).json))
      server.handler(List(path"/ws-too-large" / durable))
      val c = client(path"/ws-too-large", base, log, "large-asker")
      c.connect().sync()
      val result = c.ask[EchoRequest, EchoResponse](EchoRequest("y" * 20000), timeout = 5.seconds).attempt.sync()
      result.failed.toOption match {
        case Some(e: RpcException) => e.code shouldBe "message-too-large"
        case other                 => fail(s"expected the request refused by its id, got $other")
      }
      c.close()
    }

    "answer a message it cannot read with an error frame" in {
      val log = new InMemoryEventLog[String, ChatEvent]
      val durable = new DurableSocketServer[String, ChatEvent, ConnectInfo](config = base, eventLog = log,
        resolveChannel = (_, info) => Task.pure(info.room))
      server.handler(List(path"/ws-unreadable" / durable))
      val c = client(path"/ws-unreadable", base, log, "garbler")
      @volatile var errors: List[ErrorMessage] = Nil
      c.protocol.onError.attach(e => errors = errors :+ e)
      c.connect().sync()
      c.protocol.sendRaw("""{"type":"event","seq":7,"data":{"message": unterminated""")
      eventually(timeout(Span(5, Seconds))) {
        errors.map(_.code) should contain("unreadable")
      }
      c.close()
    }

    "refuse at the sender a message over its own limit" in {
      val log = new InMemoryEventLog[String, ChatEvent]
      val durable = new DurableSocketServer[String, ChatEvent, ConnectInfo](config = base, eventLog = log,
        resolveChannel = (_, info) => Task.pure(info.room),
        onRequest = (_, data) => Task.pure(EchoResponse(data.as[EchoRequest].text).json))
      server.handler(List(path"/ws-sender-limit" / durable))
      val c = client(path"/ws-sender-limit", base.copy(maxMessageChars = 5000), log, "limited")
      c.connect().sync()
      val result = c.ask[EchoRequest, EchoResponse](EchoRequest("z" * 10000), timeout = 5.seconds).attempt.sync()
      result.failed.toOption.map(_.getClass) shouldBe Some(classOf[IllegalArgumentException])
      c.close()
    }

    "stop the server" in {
      server.stop().sync()
      server.isRunning should be(false)
    }
  }
}
