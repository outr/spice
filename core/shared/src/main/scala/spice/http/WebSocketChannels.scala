package spice.http

import reactify.Channel

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
}