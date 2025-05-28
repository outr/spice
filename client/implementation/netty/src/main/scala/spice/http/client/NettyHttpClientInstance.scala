package spice.http.client

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http.{DefaultFullHttpRequest, HttpClientCodec, HttpContent, HttpHeaderNames, HttpHeaders, HttpObject, HttpVersion, LastHttpContent, HttpMethod => NettyHttpMethod, HttpRequest => NettyHttpRequest, HttpResponse => NettyHttpResponse}
import io.netty.handler.proxy.Socks5ProxyHandler
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import rapid.Task
import spice.http._
import spice.http.content._
import spice.net.{ContentType, Protocol, URL}

import java.io.{File, FileOutputStream}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

class NettyHttpClientInstance(client: HttpClient) extends HttpClientInstance {
  private val eventLoopGroup = new NioEventLoopGroup()

  override def send(request: HttpRequest): Task[Try[HttpResponse]] = {
    val task = Task.completable[Try[HttpResponse]]

    try {
      val uri = request.url
      val host = uri.host
      val port = uri.port
      val isHttps = uri.protocol == Protocol.Https

      val bootstrap = new Bootstrap()
        .group(eventLoopGroup)
        .channel(classOf[NioSocketChannel])
        .handler(new ChannelInitializer[Channel] {
          override def initChannel(ch: Channel): Unit = {
            val p = ch.pipeline()

            // SOCKS5 proxy support
            client.proxy match {
              case Some(pxy) if pxy.`type` == ProxyType.Socks =>
                val proxyAddress = new InetSocketAddress(pxy.host, pxy.port)
                val handler = pxy.credentials match {
                  case Some(creds) => new Socks5ProxyHandler(proxyAddress, creds.username, creds.password)
                  case None        => new Socks5ProxyHandler(proxyAddress)
                }
                p.addFirst("socks5", handler)
              case _ => // No proxy
            }

            if (isHttps) {
              val sslCtx = SslContextBuilder.forClient()
                .trustManager(InsecureTrustManagerFactory.INSTANCE)
                .build()
              p.addLast("ssl", sslCtx.newHandler(ch.alloc(), host, port))
            }

            p.addLast(new HttpClientCodec())
//            p.addLast(new LoggingHandler(LogLevel.INFO))
            p.addLast(new SimpleChannelInboundHandler[HttpObject]() {
              private var statusCode: Int = 0
              private var headersResult: Headers = Headers.empty
              private var fos: FileOutputStream = _
              private var tempFile: File = _
              private var contentType: ContentType = ContentType.`application/octet-stream`

              override def channelRead0(ctx: ChannelHandlerContext, msg: HttpObject): Unit = {
                msg match {
                  case response: NettyHttpResponse =>
                    statusCode = response.status().code()
                    val headersMap = response.headers().entries().asScala
                      .groupBy(_.getKey)
                      .view
                      .mapValues(_.map(_.getValue).toList)
                      .toMap
                    headersResult = Headers(headersMap)
                    contentType = Headers.`Content-Type`.value(headersResult).getOrElse(ContentType.`application/octet-stream`)
                    val suffix = contentType.extension.getOrElse("bin")
                    tempFile = File.createTempFile("spice", s".$suffix", new File(client.saveDirectory))
                    fos = new FileOutputStream(tempFile)

                  case chunk: HttpContent =>
                    val content = chunk.content()
                    writeToFile(content)
                    if (chunk.isInstanceOf[LastHttpContent]) {
                      fos.close()
                      val response = HttpResponse(
                        status = HttpStatus.byCode(statusCode),
                        headers = headersResult,
                        content = Some(Content.file(tempFile, contentType))
                      )
                      task.success(Success(response))
                      ctx.close()
                    }

                  case _ => // ignore
                }
              }

              override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
                try Option(fos).foreach(_.close()) catch { case _: Throwable => () }
                Option(tempFile).foreach(_.delete())
                task.failure(cause)
                ctx.close()
              }

              private def writeToFile(content: ByteBuf): Unit = {
                val bytes = new Array[Byte](content.readableBytes())
                content.readBytes(bytes)
                fos.write(bytes)
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
          val nettyUri = uri.encoded.pathAndArgs
          val method = NettyHttpMethod.valueOf(request.method.value.toUpperCase)

          val bodyBytes = request.content match {
            case Some(c) => c.asStream.toList.map(_.toArray).sync()
            case None    => Array.emptyByteArray
          }

          val nettyContent = Unpooled.wrappedBuffer(bodyBytes)
          val nettyRequest = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, method, nettyUri, nettyContent)

          nettyRequest.headers().set(HttpHeaderNames.HOST, s"$host:$port")
          nettyRequest.headers().set(HttpHeaderNames.CONTENT_LENGTH, nettyContent.readableBytes())
          nettyRequest.headers().remove(HttpHeaderNames.ORIGIN)

          request.headers.map.foreach {
            case (key, values) => values.foreach(value => nettyRequest.headers().add(key, value))
          }

          ch.writeAndFlush(nettyRequest)
        }
      })
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        task.failure(t)
    }

    task
  }

  override def webSocket(url: URL): WebSocket = new NettyHttpClientWebSocket(url, this)

  override def dispose(): Task[Unit] = Task {
    eventLoopGroup.shutdownGracefully(0, 0, TimeUnit.MILLISECONDS).sync()
  }
}