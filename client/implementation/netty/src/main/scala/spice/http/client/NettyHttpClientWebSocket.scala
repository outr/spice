package spice.http.client

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel._
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http._
import io.netty.handler.codec.http.websocketx._
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.handler.codec.http.websocketx.WebSocketClientProtocolHandler.ClientHandshakeStateEvent
import io.netty.handler.timeout.{IdleStateEvent, IdleStateHandler, ReadTimeoutException, ReadTimeoutHandler}
import reactify.Var
import rapid.Task
import spice.UserException
import spice.http.{ByteBufferData, ConnectionStatus, WebSocket}
import spice.net.URL

import java.net.URI
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

class NettyHttpClientWebSocket(url: URL, instance: NettyHttpClientInstance) extends WebSocket {
  // Use MultiThreadIoEventLoopGroup for Netty 4.2.x
  private val eventLoopGroup = new MultiThreadIoEventLoopGroup(NioIoHandler.newFactory())
  private val channelVar = Var[Option[Channel]](None)

  override def connect(): Task[ConnectionStatus] = {
    val task = Task.completable[ConnectionStatus]
    val taskCompleted = new AtomicBoolean(false)  // Track completion state

    _status @= ConnectionStatus.Connecting

    try {
      // Parse the URL string directly to preserve original format
      val uri = new URI(url.toString)

      val scheme = uri.getScheme
      if (scheme != "ws" && scheme != "wss") {
        task.failure(new IllegalArgumentException(s"Invalid WebSocket scheme: $scheme"))
        return task
      }

      val host = uri.getHost
      val port = if (uri.getPort == -1) {
        if (scheme == "wss") 443 else 80
      } else {
        uri.getPort
      }

      val ssl = scheme == "wss"
      val sslCtx = if (ssl) {
        if (instance.client.validateSSLCertificates) {
          SslContextBuilder.forClient().build()
        } else {
          SslContextBuilder.forClient()
            .trustManager(InsecureTrustManagerFactory.INSTANCE)
            .build()
        }
      } else {
        null
      }

      // Create WebSocket handshaker
      val handshaker = WebSocketClientHandshakerFactory.newHandshaker(
        uri,
        WebSocketVersion.V13,
        null,  // subprotocols
        true,  // allowExtensions
        new DefaultHttpHeaders(),
        8388608  // maxFramePayloadLength
      )

      val handler = new SimpleChannelInboundHandler[AnyRef] {
        @volatile private var handshakeComplete = false
        @volatile private var lastMessageTime = System.currentTimeMillis()

        override def channelInactive(ctx: ChannelHandlerContext): Unit = {
          val timeSinceLastMessage = System.currentTimeMillis() - lastMessageTime
          _status @= ConnectionStatus.Closed
          if (!handshakeComplete) {
            // Only fail the connection task if handshake wasn't complete
            if (taskCompleted.compareAndSet(false, true)) {
              task.failure(new RuntimeException(s"WebSocket connection to $url closed before handshake"))
            }
          }
          super.channelInactive(ctx)
        }

        override def channelRead0(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {
          lastMessageTime = System.currentTimeMillis()

          msg match {
            case frame: TextWebSocketFrame =>
              try {
                val text = frame.text()
                receive.text @= text
              } catch {
                case e: Exception => scribe.error(s"Error processing text frame", e)
              }

            case frame: BinaryWebSocketFrame =>
              try {
                // Copy the buffer since it will be released
                val buffer = frame.content().nioBuffer()
                val copy = java.nio.ByteBuffer.allocate(buffer.remaining())
                copy.put(buffer)
                copy.flip()
                receive.binary @= ByteBufferData(copy)
              } catch {
                case e: Exception =>
                  scribe.error(s"Error processing binary frame", e)
              }

            case frame: PingWebSocketFrame =>
              // Respond to ping with pong
              ctx.channel().writeAndFlush(new PongWebSocketFrame(frame.content()))

            case _: PongWebSocketFrame =>
            // Pong frames are responses to our pings

            case _: CloseWebSocketFrame =>
              _status @= ConnectionStatus.Closed
              ctx.close()

            case _ =>
              // Log unexpected frame types
              scribe.warn(s"Received unexpected WebSocket frame type: ${msg.getClass.getName}")
          }
        }

        override def userEventTriggered(ctx: ChannelHandlerContext, evt: AnyRef): Unit = {
          evt match {
            case ClientHandshakeStateEvent.HANDSHAKE_COMPLETE =>
              handshakeComplete = true
              _status @= ConnectionStatus.Open

              // Remove the read timeout handler after successful handshake
              ctx.pipeline().remove("readTimeout")

              if (taskCompleted.compareAndSet(false, true)) {
                task.success(ConnectionStatus.Open)
              }
            case ClientHandshakeStateEvent.HANDSHAKE_TIMEOUT =>
              _status @= ConnectionStatus.Closed
              if (taskCompleted.compareAndSet(false, true)) {
                task.failure(new RuntimeException(s"WebSocket handshake timeout for $url"))
              }
              ctx.close()
            case _ => super.userEventTriggered(ctx, evt)
          }
        }

        override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
          // Explicitly handle ReadTimeoutException and prevent it from reaching tail
          cause match {
            case _: ReadTimeoutException =>
              scribe.warn(s"WebSocket read timeout for $url")
              error @= cause
              _status @= ConnectionStatus.Closed
              
              if (!handshakeComplete && taskCompleted.compareAndSet(false, true)) {
                task.failure(new RuntimeException(s"WebSocket read timeout connecting to $url", cause))
              }
              
              ctx.close()
            case _ =>
              scribe.error(s"WebSocket exception", cause)
              error @= cause
              _status @= ConnectionStatus.Closed

              if (!handshakeComplete && taskCompleted.compareAndSet(false, true)) {
                task.failure(new RuntimeException(s"WebSocket error connecting to $url", cause))
              }

              ctx.close()
          }
          // Prevent exception from propagating to tail of pipeline
        }
      }

      val bootstrap = new Bootstrap()
        .group(eventLoopGroup)
        .channel(classOf[NioSocketChannel])
        .option[Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, 10000) // 10 second timeout
        .handler(new ChannelInitializer[Channel] {
          override def initChannel(ch: Channel): Unit = {
            val p = ch.pipeline()

            // SSL handler for wss://
            if (ssl && sslCtx != null) {
              p.addLast("ssl", sslCtx.newHandler(ch.alloc(), host, port))
            }

            // Add timeouts
            p.addLast("readTimeout", new ReadTimeoutHandler(60)) // 60 seconds
            p.addLast("idleState", new IdleStateHandler(0, 0, 300)) // 5 minute idle

            // HTTP codec for initial handshake
            p.addLast("httpCodec", new HttpClientCodec())
            p.addLast("httpAggregator", new HttpObjectAggregator(8388608)) // 8MB max

            // WebSocket protocol handler with larger frame size
            val config = WebSocketClientProtocolConfig.newBuilder()
              .handleCloseFrames(true)
              .dropPongFrames(false)
              .handshakeTimeoutMillis(10000L)
              .allowExtensions(true)
              .maxFramePayloadLength(8388608)
              .build()
            p.addLast("wsProtocol", new WebSocketClientProtocolHandler(handshaker, config))

            // Our custom handler
            p.addLast("wsHandler", handler)
          }
        })

      val future = bootstrap.connect(host, port)
      future.addListener((f: ChannelFuture) => {
        if (!f.isSuccess) {
          _status @= ConnectionStatus.Closed
          if (taskCompleted.compareAndSet(false, true)) {
            task.failure(new RuntimeException(s"Failed to connect to WebSocket at $url", f.cause()))
          }
        } else {
          val ch = f.channel()
          channelVar @= Some(ch)

          send.text.attach { text =>
            if (ch.isActive) {
              ch.writeAndFlush(new TextWebSocketFrame(text))
            }
          }

          send.binary.attach {
            case ByteBufferData(bb) =>
              if (ch.isActive) {
                ch.writeAndFlush(new BinaryWebSocketFrame(Unpooled.wrappedBuffer(bb)))
              }
            case other =>
              throw UserException(s"Unsupported binary frame: $other")
          }

          send.close.on {
            disconnect()
          }
        }
      })
    } catch {
      case e: Exception =>
        _status @= ConnectionStatus.Closed
        if (taskCompleted.compareAndSet(false, true)) {
          task.failure(new RuntimeException(s"Error setting up WebSocket connection to $url", e))
        }
    }

    task
  }

  override def disconnect(): Unit = {
    channelVar().foreach { ch =>
      if (ch.isActive) {
        ch.writeAndFlush(new CloseWebSocketFrame(1000, "Normal closure"))
          .addListener(ChannelFutureListener.CLOSE)
      }
    }
    _status @= ConnectionStatus.Closed

    // Schedule cleanup of event loop group
    eventLoopGroup.shutdownGracefully(0, 100, TimeUnit.MILLISECONDS)
  }
}