package spice.http.client

import cats.effect.IO
import reactify.Var
import spice.UserException
import spice.http.{ByteBufferData, ConnectionStatus, WebSocket}
import spice.net.URL

import java.net.{URI, http => jvm}
import java.nio.ByteBuffer
import java.util.concurrent.CompletionStage
import scala.concurrent.duration.DurationInt
import scala.util.Try

class JVMHttpClientWebSocket(url: URL, instance: JVMHttpClientInstance) extends jvm.WebSocket.Listener with WebSocket {
  private val jvmWebSocket = Var[Option[jvm.WebSocket]](None)

  override def connect(): IO[ConnectionStatus] = {
    _status @= ConnectionStatus.Connecting
    send.text.attach { text =>
      jvmWebSocket().foreach(ws => ws.sendText(text, true))
    }
    send.binary.attach {
      case data: ByteBufferData => jvmWebSocket().foreach(ws => ws.sendBinary(data.bb, true))
      case data => throw UserException(s"Unsupported data type: $data")
    }
    send.close.on {
      disconnect()
    }

    instance.jvmClient
      .newWebSocketBuilder()
      .buildAsync(URI.create(url.toString), this)
      .toIO
      .map { ws =>
        jvmWebSocket @= Some(ws)
      }
      .flatMap(_ => waitForConnected())
  }

  private def waitForConnected(): IO[ConnectionStatus] = status() match {
    case ConnectionStatus.Connecting => IO.sleep(100.millis).flatMap(_ => waitForConnected())
    case s => IO.pure(s)
  }

  override def disconnect(): Unit = jvmWebSocket().foreach { ws =>
    ws.abort()
    jvmWebSocket @= None
  }

  override def onOpen(webSocket: jvm.WebSocket): Unit = {
    _status @= ConnectionStatus.Open
    super.onOpen(webSocket)
  }

  override def onText(webSocket: jvm.WebSocket,
                      data: CharSequence,
                      last: Boolean): CompletionStage[_] = {
    receive.text @= data.toString
    super.onText(webSocket, data, last)
  }

  override def onBinary(webSocket: jvm.WebSocket,
                        data: ByteBuffer,
                        last: Boolean): CompletionStage[_] = {
    receive.binary @= ByteBufferData(data)
    super.onBinary(webSocket, data, last)
  }

  override def onClose(webSocket: jvm.WebSocket,
                       statusCode: Int,
                       reason: String): CompletionStage[_] = {
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
