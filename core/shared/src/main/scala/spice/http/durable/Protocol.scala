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
  reconnectStrategy: ReconnectStrategy = ReconnectStrategy.exponentialBackoff()
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
