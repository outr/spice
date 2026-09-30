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

import java.net.{ServerSocket, Socket}
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.util.Try

/**
 * A socket that never opens must not strand the client either.
 *
 * A server that takes the TCP connection and never answers the upgrade (one with no upgrade handler, or a proxy
 * holding the request) reports nothing: no error, no close. A dial waiting on it waited for good, and the first dial
 * failing, when it did fail, was re-dialled by nobody. Seen from a browser on 2026-09-30: one "connecting" line and
 * then nothing for minutes, against a development server with no upgrade handler.
 */
class DurableDialStallSpec extends AnyWordSpec with Matchers {
  private val config = DurableSocketConfig(
    reconnectStrategy = ReconnectStrategy.fixedInterval(200.millis),
    handshakeTimeout = 500.millis
  )

  /** Accepts every connection and never says a word on any of them. */
  private val holding = new ServerSocket(0)
  private val held = new CopyOnWriteArrayList[Socket]()
  private val holder = new Thread(() => {
    while (!holding.isClosed) Try(holding.accept()).foreach(held.add)
  })
  holder.setDaemon(true)
  holder.start()

  private val durableServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
    config = config,
    eventLog = new InMemoryEventLog[String, ChatEvent],
    resolveChannel = (_, info) => Task.pure(info.room)
  )

  object server extends MutableHttpServer

  private def port: Int = server.config.listeners().head.port.getOrElse(0)

  private def client(port: Int, dials: AtomicInteger): DurableSocketClient[String, ChatEvent, ConnectInfo] =
    new DurableSocketClient[String, ChatEvent, ConnectInfo](
      createWebSocket = () => {
        dials.incrementAndGet()
        HttpClient.url(url"ws://localhost".withPort(port).withPath(path"/ws")).webSocket()
      },
      config = config,
      outboundLog = new InMemoryEventLog[String, ChatEvent],
      initialChannelId = "dial-user",
      info = ConnectInfo("dial-user", "dial-room"),
      clientId = "dial-user"
    )

  "a socket that never opens" should {
    "start the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler(List(path"/ws" / durableServer))
      server.start().sync()
      server.isRunning should be(true)
    }

    "fail the first dial and keep re-dialling while nothing answers the upgrade" in {
      val dials = new AtomicInteger(0)
      val c = client(holding.getLocalPort, dials)
      val first = Try(c.connect().sync())
      first.isFailure should be(true)
      eventually(timeout(Span(10, Seconds))) { dials.get() should be >= 4 }
      c.state() should not be ProtocolState.Active
      c.close()
    }

    "keep re-dialling a port nothing listens on" in {
      val closed = new ServerSocket(0)
      val closedPort = closed.getLocalPort
      closed.close()
      val dials = new AtomicInteger(0)
      val c = client(closedPort, dials)
      Try(c.connect().sync()).isFailure should be(true)
      eventually(timeout(Span(10, Seconds))) { dials.get() should be >= 4 }
      c.close()
    }

    "connect once to a server that answers, and stay connected" in {
      val dials = new AtomicInteger(0)
      val c = client(port, dials)
      c.connect().sync()
      c.state() should be(ProtocolState.Active)
      Task.sleep(2.seconds).sync()
      c.state() should be(ProtocolState.Active)
      dials.get() should be(1)
      c.close()
    }

    "stop the server" in {
      holding.close()
      held.forEach(s => Try(s.close()))
      server.stop().sync()
      server.isRunning should be(false)
    }
  }
}
