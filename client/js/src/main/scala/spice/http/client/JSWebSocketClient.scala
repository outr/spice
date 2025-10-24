package spice.http.client

import org.scalajs.dom.{Blob, Event, MessageEvent, WebSocket => WS}
import rapid.{Fiber, Task}
import spice.http.{ByteBufferData, ConnectionStatus, WebSocket}
import spice.net.URL

import scala.concurrent.ExecutionContext
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

class JSWebSocketClient(url: URL) extends WebSocket {
  private lazy val webSocket: WS = new WS(url.toString)

  override def connect(): Task[ConnectionStatus] = {
    _status @= ConnectionStatus.Connecting
    webSocket.binaryType = "blob"
    webSocket.addEventListener("open", (_: Event) => {
      updateStatus()
    })
    webSocket.addEventListener("close", (_: Event) => {
      updateStatus()
    })
    webSocket.addEventListener("error", (_: Event) => {
      error @= new RuntimeException("WebSocket error! Closing...")
      disconnect()
      updateStatus()
    })
    webSocket.addEventListener("message", (evt: MessageEvent) => {
      evt.data match {
        case s: String => receive.text @= s
        case blob: Blob => receive.binary @= BlobData(blob)
        case ab: ArrayBuffer => receive.binary @= ByteBufferData(TypedArrayBuffer.wrap(ab))
      }
    })
    send.text.attach(webSocket.send)
    send.binary.attach {
      case BlobData(blob) => {
        val chunkSize = 1024L * 1024L * 1L     // 1 meg
        val chunks = math.ceil(blob.size / chunkSize).toInt
        val blobs = (0 until chunks).toList.map { index =>
          val start = chunkSize * index
          val end = start + chunkSize
          blob.slice(start.toDouble, end.toDouble)
        }
        blobs.foreach { b =>
          webSocket.send(b)
        }
      }
      case ByteBufferData(message) => {
        if (message.hasTypedArray()) {
          webSocket.send(message.typedArray().buffer)
        } else {
          val array = new Array[Byte](message.remaining())
          message.get(array)
          val arrayBuffer = array.toTypedArray.buffer
          webSocket.send(arrayBuffer)
        }
      }
    }
    val completable = Task.completable[ConnectionStatus]
    status.future { s =>
      s != ConnectionStatus.Connecting
    }.andThen { result =>
      completable.complete(result)
    }(ExecutionContext.global)
    completable
  }

  private def updateStatus(): Unit = webSocket.readyState match {
    case WS.CLOSED => _status @= ConnectionStatus.Closed
    case WS.CLOSING => _status @= ConnectionStatus.Closing
    case WS.CONNECTING => _status @= ConnectionStatus.Connecting
    case WS.OPEN => _status @= ConnectionStatus.Open
  }

  override def disconnect(): Unit = {
    webSocket.close()
    _status @= ConnectionStatus.Closed
  }
}