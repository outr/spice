package spice.http.server.undertow

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.undertow.Handlers
import io.undertow.server.HttpServerExchange
import io.undertow.websockets.WebSocketConnectionCallback
import io.undertow.websockets.core.{AbstractReceiveListener, BufferedBinaryMessage, BufferedTextMessage, StreamSourceFrameChannel, WebSocketChannel, WebSockets}
import io.undertow.websockets.extensions.PerMessageDeflateHandshake
import io.undertow.websockets.spi.WebSocketHttpExchange
import spice.http.server.HttpServer
import spice.http._

import scala.util.Try

object UndertowWebSocketHandler {
  def apply(undertow: HttpServerExchange,
            server: HttpServer,
            exchange: HttpExchange,
            webSocketListener: WebSocketListener): IO[Unit] = IO {
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
            scribe.error(error)
            webSocketListener.error @= error
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
        webSocketListener.connect().unsafeRunSync()
      }
    })
    if (server.config.webSocketCompression()) {
      handler.addExtension(new PerMessageDeflateHandshake)
    }
    handler.handleRequest(undertow)
  }
}