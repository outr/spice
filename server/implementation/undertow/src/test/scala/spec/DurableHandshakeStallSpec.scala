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

import scala.concurrent.duration.*

/**
 * A socket that opens and is then never answered must not strand the client.
 *
 * This is the worst shape of failure the transport has: the TCP connection is up, so nothing reports an error and
 * nothing closes, and a client waiting on the handshake waits for ever. Everything gated on `Active` — pushes, RPC,
 * anything the server wants to reach this client with — quietly goes some other way or not at all, and the client
 * still looks connected from both ends. Observed in production on 2026-09-21: sockets upgrading in a millisecond,
 * listed as connected, and not one RPC call in fifteen minutes.
 *
 * Reconnects are where it bites, which is why this stalls one rather than the first connect: a redeploy drops every
 * socket at once, so every client in the fleet re-dials into whatever the new instance is doing.
 */
class DurableHandshakeStallSpec extends AnyWordSpec with Matchers {
  private val config = DurableSocketConfig(
    ackBatchDelay = 50.millis,
    ackBatchCount = 3,
    reconnectStrategy = ReconnectStrategy.exponentialBackoff(baseDelay = 50.millis, maxDelay = 200.millis,
      maxAttempts = Int.MaxValue),
    handshakeTimeout = 500.millis
  )

  /** While set, the server accepts the socket and then never answers the handshake. */
  @volatile private var stalling = false

  /** While set, the server refuses the handshake outright, which it answers with an error frame. */
  @volatile private var refusing = false

  private val durableServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
    config = config,
    eventLog = new InMemoryEventLog[String, ChatEvent],
    resolveChannel = (_, info) =>
      if (stalling) Task.sleep(1.hour).map(_ => info.room)
      else if (refusing) Task.error(new RuntimeException("refused"))
      else Task.pure(info.room)
  )

  object server extends MutableHttpServer

  private def port: Int = server.config.listeners().head.port.getOrElse(0)

  "a handshake that is never answered" should {
    "start the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler(List(path"/ws" / durableServer))
      server.start().sync()
      server.isRunning should be(true)
    }

    "not leave the client stranded once the server answers again" in {
      val userId = "stalled-user"
      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(port).withPath(path"/ws")).webSocket(),
        config = config,
        outboundLog = new InMemoryEventLog[String, ChatEvent],
        initialChannelId = userId,
        info = ConnectInfo(userId, "stalled-room"),
        clientId = userId
      )
      client.connect().sync()
      eventually(timeout(Span(5, Seconds))) { client.state() should be(ProtocolState.Active) }

      // the instance holding this socket goes away, and whatever answers next does not answer at all
      stalling = true
      durableServer.removeSession(userId)
      client.reconnect()

      // several attempts' worth: long enough that a client which had given up would still be given up
      Task.sleep(4.seconds).sync()
      client.state() should not be ProtocolState.Active

      // and now it answers
      stalling = false
      eventually(timeout(Span(20, Seconds))) { client.state() should be(ProtocolState.Active) }
      client.close()
    }

    "recover from a handshake the server refuses" in {
      val userId = "refused-user"
      val client = new DurableSocketClient[String, ChatEvent, ConnectInfo](
        createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(port).withPath(path"/ws")).webSocket(),
        config = config,
        outboundLog = new InMemoryEventLog[String, ChatEvent],
        initialChannelId = userId,
        info = ConnectInfo(userId, "refused-room"),
        clientId = userId
      )
      client.connect().sync()
      eventually(timeout(Span(5, Seconds))) { client.state() should be(ProtocolState.Active) }

      // the next instance refuses the resume: the server answers with an error frame and nothing else.
      // Published to a channel and no further, that answer went nowhere on a reconnect -- there is no caller
      // waiting on one -- and the client stayed in `Handshaking` with its reconnect loop already spent.
      refusing = true
      durableServer.removeSession(userId)
      client.reconnect()
      Task.sleep(4.seconds).sync()
      client.state() should not be ProtocolState.Active

      refusing = false
      eventually(timeout(Span(20, Seconds))) { client.state() should be(ProtocolState.Active) }
      client.close()
    }

    "stop the server" in {
      server.stop().sync()
      server.isRunning should be(false)
    }
  }
}
