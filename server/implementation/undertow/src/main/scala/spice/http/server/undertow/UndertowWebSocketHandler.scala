package spice.http.server.undertow

import rapid._
import io.undertow.Handlers
import io.undertow.server.HttpServerExchange
import io.undertow.websockets.WebSocketConnectionCallback
import io.undertow.websockets.core.{AbstractReceiveListener, BufferedBinaryMessage, BufferedTextMessage, StreamSourceFrameChannel, WebSocketChannel, WebSockets}
import io.undertow.websockets.extensions.PerMessageDeflateHandshake
import io.undertow.websockets.spi.WebSocketHttpExchange
import spice.http.server.HttpServer
import spice.http._

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
        // Handle sending messages
        webSocketListener.send.text.attach { message =>
          WebSockets.sendText(message, channel, null)
        }
        webSocketListener.send.binary.attach {
          case ByteBufferData(message) => WebSockets.sendBinary(message, channel, null)
        }
        webSocketListener.send.close.attach { _ =>
          if (channel.isOpen) {
            Try(channel.sendClose())
          }
        }

        // Handle receiving messages
        channel.getReceiveSetter.set(new AbstractReceiveListener {
          override def onFullTextMessage(channel: WebSocketChannel, message: BufferedTextMessage): Unit = {
            webSocketListener.receive.text @= message.getData
            super.onFullTextMessage(channel, message)
          }

          override def onFullBinaryMessage(channel: WebSocketChannel, message: BufferedBinaryMessage): Unit = {
            message.getData.getResource.foreach(bb => webSocketListener.receive.binary @= ByteBufferData(bb))
            super.onFullBinaryMessage(channel, message)
          }

          override def onError(channel: WebSocketChannel, error: Throwable): Unit = {
            val remote = Option(channel.getSourceAddress).map(_.toString).getOrElse("unknown")
            if (isExpectedDisconnect(error)) {
              scribe.debug(s"WebSocket disconnected by peer: remote=$remote reason=${rootMessage(error)}")
            } else {
              scribe.error(s"WebSocket error: remote=$remote reason=${rootMessage(error)}", error)
              webSocketListener.error @= error
            }
            super.onError(channel, error)
          }

          override def onFullCloseMessage(channel: WebSocketChannel, message: BufferedBinaryMessage): Unit = {
            webSocketListener.receive.close.set(())
            super.onFullCloseMessage(channel, message)
          }

          override def onClose(webSocketChannel: WebSocketChannel, channel: StreamSourceFrameChannel): Unit = {
            webSocketListener.disconnect()
            super.onClose(webSocketChannel, channel)
          }
        })
        channel.resumeReceives()
        webSocketListener.connect().sync()
      }
    })
    if (server.config.webSocketCompression()) {
      handler.addExtension(new PerMessageDeflateHandshake)
    }
    handler.handleRequest(undertow)
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