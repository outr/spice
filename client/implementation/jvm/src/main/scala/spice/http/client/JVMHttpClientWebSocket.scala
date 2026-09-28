package spice.http.client

import rapid.{Fiber, Task}
import reactify.Var
import spice.UserException
import spice.http.{ByteBufferData, ConnectionStatus, WebSocket}
import spice.net.URL

import java.io.ByteArrayOutputStream
import java.lang.StringBuilder as TextBuilder
import java.net.{URI, http as jvm}
import java.nio.ByteBuffer
import java.util.concurrent.CompletionStage
import scala.concurrent.duration.DurationInt
import scala.util.Try

class JVMHttpClientWebSocket(url: URL, instance: JVMHttpClientInstance) extends jvm.WebSocket.Listener with WebSocket {
  private val jvmWebSocket = Var[Option[jvm.WebSocket]](None)

  override def connect(): Task[ConnectionStatus] = {
    _status @= ConnectionStatus.Connecting
    send.text.attach { text =>
      jvmWebSocket().foreach(ws => ws.sendText(text, true))
    }
    send.binary.attach {
      case data: ByteBufferData =>
        val copy = ByteBuffer.allocate(data.bb.remaining())
        copy.put(data.bb)
        copy.flip()
        jvmWebSocket().foreach(ws => ws.sendBinary(copy, true))
      case data => throw UserException(s"Unsupported data type: $data")
    }
    send.close.on {
      disconnect()
    }

    Task(instance.jvmClient.newWebSocketBuilder().buildAsync(URI.create(url.toString), this).get())
      .map { ws =>
        jvmWebSocket @= Some(ws)
      }
      .flatMap(_ => waitForConnected())
  }

  private def waitForConnected(): Task[ConnectionStatus] = status() match {
    case ConnectionStatus.Connecting => Task.sleep(100.millis).flatMap(_ => waitForConnected())
    case s => Task.pure(s)
  }

  override def disconnect(): Unit = jvmWebSocket().foreach { ws =>
    try {
      ws.sendClose(java.net.http.WebSocket.NORMAL_CLOSURE, "disconnect").join()
    } catch {
      case _: Exception => // ignore errors during close
    }
    ws.abort()
    jvmWebSocket @= None
  }

  override def onOpen(webSocket: jvm.WebSocket): Unit = {
    _status @= ConnectionStatus.Open
    super.onOpen(webSocket)
  }

  // The JDK hands a long message over in parts, `last` marking its end: the parts so far of the one in progress.
  private val textParts = new TextBuilder
  private val binaryParts = new ByteArrayOutputStream

  /** A message is received once, whole: a part is kept until the one marked `last` completes it. */
  override def onText(webSocket: jvm.WebSocket,
                      data: CharSequence,
                      last: Boolean): CompletionStage[?] = {
    if (last && textParts.length == 0) receive.text @= data.toString
    else {
      textParts.append(data)
      if (last) {
        val text = textParts.toString
        textParts.setLength(0)
        receive.text @= text
      }
    }
    super.onText(webSocket, data, last)
  }

  override def onBinary(webSocket: jvm.WebSocket,
                        data: ByteBuffer,
                        last: Boolean): CompletionStage[?] = {
    if (last && binaryParts.size == 0) receive.binary @= ByteBufferData(data)
    else {
      // A part's buffer is the JDK's to reuse once this returns: its bytes are copied out.
      val bytes = new Array[Byte](data.remaining())
      data.get(bytes)
      binaryParts.write(bytes)
      if (last) {
        val whole = ByteBuffer.wrap(binaryParts.toByteArray)
        binaryParts.reset()
        receive.binary @= ByteBufferData(whole)
      }
    }
    super.onBinary(webSocket, data, last)
  }

  override def onClose(webSocket: jvm.WebSocket,
                       statusCode: Int,
                       reason: String): CompletionStage[?] = {
    _status @= ConnectionStatus.Closed
    super.onClose(webSocket, statusCode, reason)
  }

  override def onError(webSocket: jvm.WebSocket,
                       error: Throwable): Unit = {
    this.error @= error
    Try(disconnect())
    _status @= ConnectionStatus.Closed
    super.onError(webSocket, error)
  }
}
