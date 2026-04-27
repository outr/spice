package spice.http.server.undertow

import rapid.*
import io.undertow.Handlers
import io.undertow.server.HttpServerExchange
import io.undertow.websockets.WebSocketConnectionCallback
import io.undertow.websockets.core.{AbstractReceiveListener, BufferedBinaryMessage, BufferedTextMessage, StreamSourceFrameChannel, WebSocketChannel, WebSockets}
import io.undertow.websockets.extensions.PerMessageDeflateHandshake
import io.undertow.websockets.spi.WebSocketHttpExchange
import spice.http.server.HttpServer
import spice.http.*

import java.net.SocketException
import java.nio.channels.ClosedChannelException
import scala.util.Try

object UndertowWebSocketHandler {
  def apply(undertow: HttpServerExchange,
            server: HttpServer,
            exchange: HttpExchange,
            webSocketListener: WebSocketListener): Task[Unit] = Task {
    val handler = Handlers.websocket(new WebSocketConnectionCallback {
      override def onConnect(exchange: WebSocketHttpExchange, channel: WebSocketChannel): Unit = {
        // Disable Undertow's idle timeout — application-level heartbeats handle liveness
        channel.setIdleTimeout(-1L)
        // Handle sending messages
        webSocketListener.send.text.attach { message =>
          val len = message.length
          if (len > 1_000_000) {
            scribe.warn(s"[WS-SEND] Large text frame: ${len} bytes (${len / 1024}KB). Preview: ${message.take(200)}...")
          }
          safely(s"send.text dispatch — len=$len preview='${textPreview(message)}'") {
            WebSockets.sendText(message, channel, null)
          }
        }
        webSocketListener.send.binary.attach {
          case ByteBufferData(message) =>
            val len = message.remaining()
            if (len > 1_000_000) {
              scribe.warn(s"[WS-SEND] Large binary frame: ${len} bytes (${len / 1024}KB)")
            }
            safely(s"send.binary dispatch — ${binaryPreview(message)}") {
              WebSockets.sendBinary(message, channel, null)
            }
        }
        webSocketListener.send.close.attach { _ =>
          if (channel.isOpen) {
            Try(channel.sendClose())
          }
        }

        // Handle receiving messages
        channel.getReceiveSetter.set(new AbstractReceiveListener {
          override def onFullTextMessage(channel: WebSocketChannel, message: BufferedTextMessage): Unit = {
            val data = message.getData
            val len = data.length
            if (len > 1_000_000) {
              scribe.warn(s"[WS-RECV] Large text frame: ${len} bytes (${len / 1024}KB). Preview: ${data.take(200)}...")
            }
            safely(s"receive.text listener — len=$len preview='${textPreview(data)}'") {
              webSocketListener.receive.text @= data
            }
            super.onFullTextMessage(channel, message)
          }

          override def onFullBinaryMessage(channel: WebSocketChannel, message: BufferedBinaryMessage): Unit = {
            val buffers = message.getData.getResource
            buffers.foreach { bb =>
              safely(s"receive.binary listener — ${binaryPreview(bb)}") {
                webSocketListener.receive.binary @= ByteBufferData(bb)
              }
            }
            super.onFullBinaryMessage(channel, message)
          }

          override def onError(channel: WebSocketChannel, error: Throwable): Unit = {
            val remote = Option(channel.getSourceAddress).map(_.toString).getOrElse("unknown")
            if (isExpectedDisconnect(error)) {
              scribe.debug(s"WebSocket disconnected by peer: remote=$remote reason=${rootMessage(error)}")
            } else {
              scribe.error(s"WebSocket error: remote=$remote reason=${rootMessage(error)}", error)
              safely("error listener")(webSocketListener.error @= error)
            }
            super.onError(channel, error)
          }

          override def onFullCloseMessage(channel: WebSocketChannel, message: BufferedBinaryMessage): Unit = {
            safely("receive.close listener")(webSocketListener.receive.close.set(()))
            super.onFullCloseMessage(channel, message)
          }

          override def onClose(webSocketChannel: WebSocketChannel, channel: StreamSourceFrameChannel): Unit = {
            safely("listener disconnect")(webSocketListener.disconnect())
            super.onClose(webSocketChannel, channel)
          }
        })
        channel.resumeReceives()
        safely("listener connect")(webSocketListener.connect().sync())
      }
    })
    if (server.config.webSocketCompression()) {
      handler.addExtension(new PerMessageDeflateHandshake)
    }
    handler.handleRequest(undertow)
  }

  /** Run user-supplied listener code, logging any exception with full stack trace
    * instead of letting it propagate up into Undertow's channel listener invoker
    * (which produces opaque "XNIO001007" errors with no useful context).
    * The `context` is by-name so message-preview formatting only happens on failure. */
  private def safely(context: => String)(body: => Unit): Unit = {
    try body
    catch {
      case t: Throwable =>
        scribe.error(s"WebSocket listener threw in [$context]: ${t.getClass.getName}: ${rootMessage(t)}", t)
    }
  }

  private def textPreview(data: String): String = {
    val cleaned = data.replace('\n', ' ').replace('\r', ' ')
    if (cleaned.length <= 300) cleaned else s"${cleaned.take(300)}…(+${cleaned.length - 300} bytes)"
  }

  private def binaryPreview(bb: java.nio.ByteBuffer): String = {
    val total = bb.remaining()
    val sample = math.min(total, 64)
    val dup = bb.duplicate()
    val bytes = new Array[Byte](sample)
    dup.get(bytes)
    val hex = bytes.map(b => f"${b & 0xff}%02x").mkString(" ")
    if (total <= sample) s"len=$total hex=[$hex]" else s"len=$total hex=[$hex …]"
  }

  private def isExpectedDisconnect(error: Throwable): Boolean = {
    val message = Option(error.getMessage).getOrElse("").toLowerCase
    error match {
      case _: ClosedChannelException => true
      case se: SocketException =>
        message.contains("connection reset") || message.contains("broken pipe") || message.contains("connection closed")
      case _ =>
        Option(error.getCause).exists(isExpectedDisconnect)
    }
  }

  private def rootMessage(error: Throwable): String = {
    @annotation.tailrec
    def loop(t: Throwable): Throwable = Option(t.getCause) match {
      case Some(cause) => loop(cause)
      case None => t
    }
    val root = loop(error)
    Option(root.getMessage).filter(_.nonEmpty).getOrElse(root.getClass.getSimpleName)
  }
}