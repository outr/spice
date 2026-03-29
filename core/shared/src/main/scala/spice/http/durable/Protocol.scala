package spice.http.durable

import fabric.Json
import fabric.rw.*

import scala.concurrent.duration.*

// Messages that consume sequence numbers (assigned by sender's EventLog)
case class InvokeMessage(seq: Long, id: String, tool: String, args: Json)
object InvokeMessage {
  given rw: RW[InvokeMessage] = RW.gen
}

case class ResultMessage(seq: Long, id: String, data: Json, error: Option[String] = None)
object ResultMessage {
  given rw: RW[ResultMessage] = RW.gen
}

case class EventMessage(seq: Long, event: String, data: Json)
object EventMessage {
  given rw: RW[EventMessage] = RW.gen
}

// Control messages (no seq — not logged)
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

case class PingMessage(ts: Long)
object PingMessage {
  given rw: RW[PingMessage] = RW.gen
}

case class PongMessage(ts: Long)
object PongMessage {
  given rw: RW[PongMessage] = RW.gen
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

case class DurableSocketConfig(
  ackBatchDelay: FiniteDuration = 100.millis,
  ackBatchCount: Int = 10,
  resendTimeout: FiniteDuration = 5.seconds,
  heartbeatInterval: FiniteDuration = 30.seconds,
  heartbeatTimeout: FiniteDuration = 90.seconds,
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
