package spice.http.client

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http.{
  HttpClientCodec,
  HttpObjectAggregator,
  HttpRequest => NettyHttpRequest,
  HttpResponse => NettyHttpResponse,
  HttpMethod => NettyHttpMethod,
  HttpVersion,
  DefaultFullHttpRequest,
  HttpHeaderNames,
  FullHttpResponse
}
import io.netty.handler.proxy.Socks5ProxyHandler
import rapid.Task
import spice.http._
import spice.http.content.BytesContent
import spice.net.{ContentType, URL}

import java.net.InetSocketAddress
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

class NettyHttpClientInstance(client: HttpClient) extends HttpClientInstance {
  private val eventLoopGroup = new NioEventLoopGroup()

  override def send(request: HttpRequest): Task[Try[HttpResponse]] = {
    val task = Task.completable[Try[HttpResponse]]

    try {
      val uri = request.url
      val host = uri.host
      val port = uri.port

      val bootstrap = new Bootstrap()
        .group(eventLoopGroup)
        .channel(classOf[NioSocketChannel])
        .handler(new ChannelInitializer[Channel] {
          override def initChannel(ch: Channel): Unit = {
            val p = ch.pipeline()

            client.proxy match {
              case Some(pxy) if pxy.`type` == ProxyType.Socks =>
                val proxyAddress = new InetSocketAddress(pxy.host, pxy.port)
                val handler = pxy.credentials match {
                  case Some(creds) =>
                    new Socks5ProxyHandler(proxyAddress, creds.username, creds.password)
                  case None =>
                    new Socks5ProxyHandler(proxyAddress)
                }
                p.addFirst("socks5", handler)
              case _ => // no proxy
            }

            p.addLast(new HttpClientCodec())
            p.addLast(new HttpObjectAggregator(1048576)) // 1MB max
            p.addLast(new SimpleChannelInboundHandler[FullHttpResponse]() {
              override def channelRead0(ctx: ChannelHandlerContext, msg: FullHttpResponse): Unit = {
                val contentBytes = new Array[Byte](msg.content().readableBytes())
                msg.content().readBytes(contentBytes)

                val headersMap = msg.headers().entries().asScala
                  .groupBy(_.getKey)
                  .view
                  .mapValues(_.map(_.getValue).toList)
                  .toMap
                val headersResult = Headers(headersMap)
                val contentType = Headers.`Content-Type`.value(headersResult).getOrElse(ContentType.`text/plain`)

                task.success(Success(HttpResponse(
                  status = HttpStatus.byCode(msg.status().code()),
                  headers = headersResult,
                  content = Some(BytesContent(contentBytes, contentType))
                )))
              }

              override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
                task.failure(cause)
                ctx.close()
              }
            })
          }
        })

      val chFuture = bootstrap.connect(host, port)
      chFuture.addListener((f: ChannelFuture) => {
        if (!f.isSuccess) {
          task.failure(f.cause())
        } else {
          val ch = f.channel()
          val nettyUri = uri.toString.replace("{", "%7B").replace("}", "%7D")
          val method = NettyHttpMethod.valueOf(request.method.value.toUpperCase)

          val bodyBytes = request.content match {
            case Some(c) => c.asStream.toList.map(_.toArray).sync()
            case None => Array.emptyByteArray
          }

          val nettyContent = Unpooled.wrappedBuffer(bodyBytes)
          val nettyRequest = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, method, nettyUri, nettyContent)

          nettyRequest.headers().set(HttpHeaderNames.HOST, host)
          nettyRequest.headers().set(HttpHeaderNames.CONTENT_LENGTH, nettyContent.readableBytes())

          request.headers.map.foreach {
            case (key, values) =>
              values.foreach(value => nettyRequest.headers().add(key, value))
          }

          ch.writeAndFlush(nettyRequest)
        }
      })
    } catch {
      case t: Throwable => task.failure(t)
    }

    task
  }

  override def webSocket(url: URL): WebSocket = new NettyHttpClientWebSocket(url, this)

  override def dispose(): Task[Unit] = Task {
    eventLoopGroup.shutdownGracefully()
  }
}