package spice.http.client

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http._
import io.netty.handler.codec.http.websocketx._
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.handler.codec.http.websocketx.WebSocketClientProtocolHandler.ClientHandshakeStateEvent
import reactify.Var
import rapid.Task
import spice.UserException
import spice.http.{ByteBufferData, ConnectionStatus, WebSocket}
import spice.net.URL

import java.net.URI
import java.nio.ByteBuffer
import scala.concurrent.duration.DurationInt

class NettyHttpClientWebSocket(url: URL, instance: NettyHttpClientInstance) extends WebSocket {
  private val eventLoopGroup = new NioEventLoopGroup()
  private val webSocketVar = Var[Option[WebSocketClientHandshaker]](None)
  private val channelVar = Var[Option[Channel]](None)

  override def connect(): Task[ConnectionStatus] = {
    val task = Task.completable[ConnectionStatus]
    _status @= ConnectionStatus.Connecting

    val uri = new URI(
      url.protocol.scheme,
      null,
      url.host,
      url.port,
      url.path.encoded,
      if (url.parameters.isEmpty) null else url.parameters.encoded.drop(1),
      url.fragment.orNull
    )
    val scheme = uri.getScheme
    val host = uri.getHost
    val port = if (uri.getPort == -1) {
      if (scheme == "wss") 443 else 80
    } else {
      uri.getPort
    }

    val ssl = scheme == "wss"
    val sslCtx =
      if (ssl)
        SslContextBuilder.forClient().trustManager(InsecureTrustManagerFactory.INSTANCE).build()
      else
        null

    val handshaker = WebSocketClientHandshakerFactory.newHandshaker(
      uri,
      WebSocketVersion.V13,
      null,
      true,
      new DefaultHttpHeaders()
    )

    val handler = new SimpleChannelInboundHandler[AnyRef] {
      override def channelRead0(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
        msg match {
          case frame: TextWebSocketFrame =>
            receive.text @= frame.text()
          case frame: BinaryWebSocketFrame =>
            receive.binary @= ByteBufferData(frame.content().nioBuffer())
          case _: PongWebSocketFrame => // ignore
          case _: CloseWebSocketFrame =>
            _status @= ConnectionStatus.Closed
            ctx.close()
          case _ => // unknown
        }
      }

      override def channelActive(ctx: ChannelHandlerContext): Unit = {
//        handshaker.handshake(ctx.channel())
      }

      override def userEventTriggered(ctx: ChannelHandlerContext, evt: AnyRef): Unit = {
        if (evt == ClientHandshakeStateEvent.HANDSHAKE_COMPLETE) {
          _status @= ConnectionStatus.Open
          task.success(ConnectionStatus.Open)
        } else {
          super.userEventTriggered(ctx, evt)
        }
      }

      override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
        error @= cause
        _status @= ConnectionStatus.Closed
        task.failure(cause)
        ctx.close()
      }
    }

    val bootstrap = new Bootstrap()
      .group(eventLoopGroup)
      .channel(classOf[NioSocketChannel])
      .handler(new ChannelInitializer[Channel] {
        override def initChannel(ch: Channel): Unit = {
          val p = ch.pipeline()
          if (ssl && sslCtx != null) {
            p.addLast(sslCtx.newHandler(ch.alloc(), host, port))
          }
          p.addLast(new HttpClientCodec())
          p.addLast(new HttpObjectAggregator(8192))
          p.addLast(new WebSocketClientProtocolHandler(handshaker))
          p.addLast(handler)
        }
      })

    val future = bootstrap.connect(host, port)
    future.addListener((f: ChannelFuture) => {
      if (!f.isSuccess) {
        _status @= ConnectionStatus.Closed
        task.success(ConnectionStatus.Closed)
      } else {
        val ch = f.channel()

        channelVar @= Some(ch)
        webSocketVar @= Some(handshaker)

        send.text.attach { text =>
          ch.writeAndFlush(new TextWebSocketFrame(text))
        }

        send.binary.attach {
          case ByteBufferData(bb) => ch.writeAndFlush(new BinaryWebSocketFrame(Unpooled.wrappedBuffer(bb)))
          case other              => throw UserException(s"Unsupported binary frame: $other")
        }

        send.close.on {
          disconnect()
        }
      }
    })

    task
  }

  override def disconnect(): Unit = channelVar().foreach { ch =>
    ch.writeAndFlush(new CloseWebSocketFrame())
    ch.close()
    _status @= ConnectionStatus.Closed
  }
}