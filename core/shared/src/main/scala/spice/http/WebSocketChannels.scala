package spice.http

import reactify.Channel

/** A coded WebSocket close: an RFC 6455 status code plus reason (e.g. 1001 "going away" on a graceful
  * drain), as opposed to the uncoded normal close carried by [[WebSocketChannels.close]]. */
case class CloseReason(code: Int, reason: String)

class WebSocketChannels {
  val text: Channel[String] = new Channel[String] {
    override protected def synchronize: Boolean = true
  }
  val binary: Channel[BinaryData] = new Channel[BinaryData] {
    override protected def synchronize: Boolean = true
  }
  val close: Channel[Unit] = new Channel[Unit] {
    override protected def synchronize: Boolean = true
  }
  /** Coded close (status code + reason). Distinct from [[close]] (an uncoded normal close) so a graceful
    * drain can send 1001 "going away". */
  val closeWith: Channel[CloseReason] = new Channel[CloseReason] {
    override protected def synchronize: Boolean = true
  }
}
