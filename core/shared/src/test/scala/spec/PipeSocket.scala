package spec

import rapid.Task
import spice.http.{ByteBufferData, ConnectionStatus, WebSocket}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

/** One end of a socket pair in memory: what one end sends, the other receives, in order and a turn later (never on
  * the sender's stack), as it would over a wire. */
class PipeSocket extends WebSocket {
  @volatile var peer: PipeSocket = scala.compiletime.uninitialized
  private val inbox = new ConcurrentLinkedQueue[() => Unit]()
  private val draining = new AtomicBoolean(false)

  send.text.attach(text => peer.post(() => peer.receive.text @= text))
  send.binary.attach {
    case ByteBufferData(bb) =>
      val copy = java.nio.ByteBuffer.allocate(bb.remaining())
      copy.put(bb.duplicate()).flip()
      peer.post(() => peer.receive.binary @= ByteBufferData(copy))
    case _ => ()
  }

  private def post(message: () => Unit): Unit = {
    inbox.add(message)
    drain()
  }

  private def drain(): Unit = if (draining.compareAndSet(false, true)) {
    Task {
      var next = inbox.poll()
      while (next != null) {
        next()
        next = inbox.poll()
      }
      draining.set(false)
      if (!inbox.isEmpty) drain()
    }.start()
    ()
  }

  def open(): Unit = _status @= ConnectionStatus.Open

  override def connect(): Task[ConnectionStatus] = Task {
    open()
    ConnectionStatus.Open
  }

  override def disconnect(): Unit = _status @= ConnectionStatus.Closed
}

object PipeSocket {
  def pair(): (PipeSocket, PipeSocket) = {
    val a = new PipeSocket
    val b = new PipeSocket
    a.peer = b
    b.peer = a
    a.open()
    b.open()
    (a, b)
  }
}
