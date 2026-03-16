package spice.http

import reactify.{Channel, Val}

import java.util.concurrent.ConcurrentLinkedQueue

/**
 * WebSocketChannels that buffers outgoing text and binary messages when the
 * connection is not yet open. Buffered messages are flushed in order as soon
 * as the status transitions to Open.
 */
class BufferedWebSocketChannels(connectionStatus: Val[ConnectionStatus]) extends WebSocketChannels {
  private val pendingText = new ConcurrentLinkedQueue[String]()
  private val pendingBinary = new ConcurrentLinkedQueue[BinaryData]()

  private def isOpen: Boolean = connectionStatus() == ConnectionStatus.Open

  override val text: Channel[String] = new Channel[String] {
    override protected def synchronize: Boolean = true

    override def set(f: => String): Unit = {
      if (isOpen) {
        super.set(f)
      } else {
        pendingText.add(f)
      }
    }
  }

  override val binary: Channel[BinaryData] = new Channel[BinaryData] {
    override protected def synchronize: Boolean = true

    override def set(f: => BinaryData): Unit = {
      if (isOpen) {
        super.set(f)
      } else {
        pendingBinary.add(f)
      }
    }
  }

  // Flush pending messages when connection opens
  connectionStatus.attach {
    case ConnectionStatus.Open => flush()
    case _ => // ignore
  }

  private def flush(): Unit = {
    var msg = pendingText.poll()
    while (msg != null) {
      text.set(msg)
      msg = pendingText.poll()
    }
    var bin = pendingBinary.poll()
    while (bin != null) {
      binary.set(bin)
      bin = pendingBinary.poll()
    }
  }
}
