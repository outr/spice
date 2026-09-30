package spec

import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.*
import spice.http.{ConnectionStatus, WebSocket}
import spice.http.durable.*

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.util.Try

/**
 * The re-dial loop against transports that go nowhere, each with its own way of going nowhere: one that never opens,
 * one refused at once, and one that opens and is never answered -- whose close is reported the moment it is dropped,
 * as a browser's is.
 */
class DurableRedialSpec extends AnyWordSpec with Matchers {
  private val config = DurableSocketConfig(
    reconnectStrategy = ReconnectStrategy.fixedInterval(300.millis),
    handshakeTimeout = 200.millis
  )

  /** A transport whose `connect` settles on `opens`: never (`None`), or on the status given. */
  private class FakeSocket(opens: Option[ConnectionStatus]) extends WebSocket {
    override def connect(): Task[ConnectionStatus] = {
      _status @= ConnectionStatus.Connecting
      opens match {
        case Some(status) =>
          _status @= status
          Task.pure(status)
        case None => Task.completable[ConnectionStatus]
      }
    }

    override def disconnect(): Unit = _status @= ConnectionStatus.Closed
  }

  private def dialsIn(opens: Option[ConnectionStatus], window: FiniteDuration): Int = {
    val dials = new AtomicInteger(0)
    val client = new DurableSocketClient[String, String, String](
      createWebSocket = () => {
        dials.incrementAndGet()
        new FakeSocket(opens)
      },
      config = config,
      outboundLog = new InMemoryEventLog[String, String],
      initialChannelId = "channel",
      info = "info"
    )
    client.connect().start()
    Task.sleep(window).sync()
    client.close()
    dials.get()
  }

  "the durable client" should {
    "re-dial a socket that never opens" in {
      // a dial every 500 ms: 200 ms to give up on it, 300 ms before the next
      dialsIn(None, 2200.millis) should (be >= 3 and be <= 5)
    }
    "re-dial a socket refused at once, the first dial included" in {
      // a dial every 300 ms
      dialsIn(Some(ConnectionStatus.Closed), 2000.millis) should (be >= 5 and be <= 7)
    }
    "re-dial a handshake never answered, one dial at a time" in {
      // a dial every 500 ms; two loops running would make it twice that
      dialsIn(Some(ConnectionStatus.Open), 2200.millis) should (be >= 3 and be <= 5)
    }
    "fail the first dial when the socket will not open" in {
      val client = new DurableSocketClient[String, String, String](
        createWebSocket = () => new FakeSocket(Some(ConnectionStatus.Closed)),
        config = config.copy(reconnectStrategy = ReconnectStrategy.none),
        outboundLog = new InMemoryEventLog[String, String],
        initialChannelId = "channel",
        info = "info"
      )
      Try(client.connect().sync()).isFailure should be(true)
      client.state() should be(ProtocolState.Disconnected)
    }
  }
}
