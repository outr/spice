package spice.http.client

import okhttp3.{OkHttpClient, Request, Response, WebSocketListener}
import okio.ByteString
import rapid.Task
import spice.http.*
import spice.net.URL

import scala.concurrent.duration.DurationInt
import scala.util.Try

class OkHttpWebSocket(url: URL, instance: OkHttpClient) extends WebSocketListener with WebSocket {
  private lazy val ws: okhttp3.WebSocket = {
    val request = new Request.Builder().url(url.toString).build()
    instance.newWebSocket(request, this)
  }

  override def connect(): Task[ConnectionStatus] = Task {
    _status @= ConnectionStatus.Connecting
    ws
    send.text.attach { text =>
      ws.send(text)
    }
    send.binary.attach {
      case data: ByteBufferData => ws.send(ByteString.of(data.bb))
      case data => throw new RuntimeException(s"Unsupported data type: $data")
    }
    send.close.on {
      ws.close(1000, null)
    }
  }.flatMap(_ => waitForConnected())

  private def waitForConnected(): Task[ConnectionStatus] = status() match {
    case ConnectionStatus.Connecting => Task.sleep(100.millis).flatMap(_ => waitForConnected())
    case s => Task.pure(s)
  }

  override def disconnect(): Unit = ws.close(1000, "disconnect requested")

  override def onClosed(webSocket: okhttp3.WebSocket,
                        code: Int,
                        reason: String): Unit = _status @= ConnectionStatus.Closed

  override def onClosing(webSocket: okhttp3.WebSocket,
                         code: Int,
                         reason: String): Unit = _status @= ConnectionStatus.Closing

  override def onFailure(webSocket: okhttp3.WebSocket,
                         t: Throwable,
                         response: Response): Unit = {
    error @= t
    Try(ws.close(1, t.getMessage))
    _status @= ConnectionStatus.Closed
  }

  override def onMessage(webSocket: okhttp3.WebSocket,
                         text: String): Unit = receive.text @= text

  override def onMessage(webSocket: okhttp3.WebSocket,
                         bytes: ByteString): Unit = receive.binary @= ByteBufferData(bytes.asByteBuffer())

  override def onOpen(webSocket: okhttp3.WebSocket,
                      response: Response): Unit = _status @= ConnectionStatus.Open
}