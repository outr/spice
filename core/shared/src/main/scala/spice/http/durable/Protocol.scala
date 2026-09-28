package spice.http.durable

import fabric.Json
import fabric.rw.*

import scala.concurrent.duration.*

// Durable event message (sequenced, logged, replayed)
case class EventMessage(seq: Long, data: Json)
object EventMessage {
  given rw: RW[EventMessage] = RW.gen
}

// Ephemeral control messages (not logged)
case class ConnectMessage(clientId: String, info: Json)
object ConnectMessage {
  given rw: RW[ConnectMessage] = RW.gen
}

case class ResumeMessage(clientId: String, lastSeq: Long, info: Json)
object ResumeMessage {
  given rw: RW[ResumeMessage] = RW.gen
}

case class ConnectedMessage(lastClientSeq: Long, resumed: Boolean)
object ConnectedMessage {
  given rw: RW[ConnectedMessage] = RW.gen
}

case class AckMessage(seq: Long)
object AckMessage {
  given rw: RW[AckMessage] = RW.gen
}

case class ErrorMessage(code: String, message: String)
object ErrorMessage {
  given rw: RW[ErrorMessage] = RW.gen
}

case class SwitchMessage(channelId: Json, lastSeq: Long)
object SwitchMessage {
  given rw: RW[SwitchMessage] = RW.gen
}

case class SwitchedMessage(channelId: Json, lastSeq: Long)
object SwitchedMessage {
  given rw: RW[SwitchedMessage] = RW.gen
}

// RPC request/response (ephemeral, correlated by id — never logged or replayed). `data` is any
// RW'able payload; when paired with a polymorphic Event hierarchy, the request payload's own
// discriminator selects the server handler, so no method name rides the wire.
case class RequestMessage(id: Long, data: Json)
object RequestMessage {
  given rw: RW[RequestMessage] = RW.gen
}

case class ResponseMessage(id: Long, data: Json)
object ResponseMessage {
  given rw: RW[ResponseMessage] = RW.gen
}

case class ResponseErrorMessage(id: Long, code: String, message: String)
object ResponseErrorMessage {
  given rw: RW[ResponseErrorMessage] = RW.gen
}

case class DurableSocketConfig(
  ackBatchDelay: FiniteDuration = 100.millis,
  ackBatchCount: Int = 10,
  reconnectStrategy: ReconnectStrategy = ReconnectStrategy.exponentialBackoff(),
  /** How long a handshake may go unanswered before the connection is given up on and re-dialled.
    *
    * A socket that opens and is then never answered is the worst shape of failure available here: the transport
    * is up, so nothing reports an error and nothing closes, and the client waits in `Handshaking` for ever while
    * everything it would have sent over the socket quietly goes by other means instead. A deadline turns that
    * into an ordinary reconnect. */
  handshakeTimeout: FiniteDuration = 15.seconds,
  /** Characters above which a text message is sent as `chunk` frames, reassembled by the peer before it is read.
    *
    * Only to a peer that said in its handshake it reads chunks; to any other the message goes whole as before.
    * Chunking keeps each frame under the caps websocket servers and proxies put on a frame; it does not change the
    * size of the message the peer reads, which [[maxMessageChars]] bounds. */
  maxFrameChars: Int = 1_000_000,
  /** The longest message, in characters, either side sends or reads — whole or reassembled from chunks.
    *
    * A message over it is refused before it is parsed: the peer gets an `error` frame (a `response-error` for a
    * request), and a send over it fails at the sender, so neither end holds an unbounded message in memory and
    * neither waits on one that will never be answered. */
  maxMessageChars: Int = 64_000_000
)

trait ReconnectStrategy {
  def nextDelay(attempt: Int): Option[FiniteDuration]
}

object ReconnectStrategy {
  def exponentialBackoff(
    baseDelay: FiniteDuration = 1.second,
    maxDelay: FiniteDuration = 30.seconds,
    maxAttempts: Int = 20
  ): ReconnectStrategy = new ReconnectStrategy {
    override def nextDelay(attempt: Int): Option[FiniteDuration] = {
      if (attempt >= maxAttempts) None
      else {
        val delay = baseDelay * Math.pow(2, attempt.min(10)).toLong
        Some(if (delay > maxDelay) maxDelay else delay)
      }
    }
  }

  def fixedInterval(delay: FiniteDuration = 10.seconds): ReconnectStrategy = new ReconnectStrategy {
    override def nextDelay(attempt: Int): Option[FiniteDuration] = Some(delay)
  }

  def none: ReconnectStrategy = new ReconnectStrategy {
    override def nextDelay(attempt: Int): Option[FiniteDuration] = None
  }
}

enum ProtocolState {
  case Disconnected, Handshaking, Active, Reconnecting, Closed
}
