package spice.http.client

import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel._
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.pool.SimpleChannelPool
import io.netty.handler.proxy.{HttpProxyHandler, Socks5ProxyHandler}
import io.netty.handler.codec.http.{DefaultFullHttpRequest, HttpClientCodec, HttpContent, HttpHeaderNames, HttpHeaderValues, HttpHeaders, HttpObject, HttpVersion, LastHttpContent, HttpMethod => NettyHttpMethod, HttpRequest => NettyHttpRequest, HttpResponse => NettyHttpResponse}
import io.netty.util.concurrent.{Future => NettyFuture}
import rapid.Task
import spice.http.{HttpRequest, HttpResponse, _}
import spice.http.content._
import spice.net.{ContentType, Protocol, URL}
import fabric.io.JsonFormatter
import io.netty.bootstrap.Bootstrap
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.handler.timeout.{IdleStateHandler, ReadTimeoutHandler, WriteTimeoutHandler}
import rapid.task.Completable
import spice.http.content.FormDataEntry.{FileEntry, StringEntry}

import java.io.{ByteArrayOutputStream, File, FileOutputStream}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

class NettyHttpClientInstance(val client: HttpClient) extends HttpClientInstance {
  private val eventLoopGroup = new MultiThreadIoEventLoopGroup(NioIoHandler.newFactory())

  // Initialize connection pool manager
  private val poolManager = new NettyConnectionPoolManager(
    eventLoopGroup,
    client,
    client.connectionPool.maxIdleConnections,
    client.connectionPool.keepAlive
  )

  override def send(request: HttpRequest): Task[Try[HttpResponse]] = {
    val task = Task.completable[Try[HttpResponse]]

    try {
      // Check if we should use pooling
      if (client.proxy.exists(_.`type` != ProxyType.Direct)) {
        // For proxy connections, create a new channel each time
        sendViaNewChannel(request, task)
      } else {
        // Use connection pooling for direct connections
        sendViaPool(request, task)
      }
    } catch {
      case t: Throwable =>
        task.success(Failure(t))
    }

    task
  }

  private def sendViaPool(request: HttpRequest, task: Completable[Try[HttpResponse]]): Unit = {
    val uri = request.url
    val host = uri.host
    val port = uri.port
    val isHttps = uri.protocol == Protocol.Https

    // Get connection pool for this host/port
    val pool = poolManager.getPool(host, port, isHttps)

    // Acquire a channel from the pool
    val acquireFuture = pool.acquire()
    acquireFuture.addListener((future: NettyFuture[Channel]) => {
      if (!future.isSuccess) {
        task.success(Failure(future.cause()))
      } else {
        val channel = future.getNow

        try {
          // Create response handler with a unique name to allow safe removal
          val handlerName = s"responseHandler-${System.nanoTime()}"
          val responseHandler = new PooledResponseHandler(task, client, pool, channel, handlerName)
          channel.pipeline().addLast(handlerName, responseHandler)

          // Create and send request
          val nettyRequest = createNettyRequest(request, uri)
          channel.writeAndFlush(nettyRequest)
        } catch {
          case t: Throwable =>
            try {
              pool.release(channel)
            } catch {
              case _: Throwable => // Ignore cleanup errors
            }
            task.success(Failure(t))
        }
      }
    })
  }

  private def sendViaNewChannel(request: HttpRequest, task: Completable[Try[HttpResponse]]): Unit = {
    val uri = request.url
    val host = uri.host
    val port = uri.port
    val isHttps = uri.protocol == Protocol.Https

    val bootstrap = new Bootstrap()
      .group(eventLoopGroup)
      .channel(classOf[NioSocketChannel])
      .option[Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, client.timeout.toMillis.toInt)
      .remoteAddress(new InetSocketAddress(host, port)) // Connect to TARGET host/port, not proxy!

    bootstrap.handler(new ChannelInitializer[Channel] {
      override def initChannel(ch: Channel): Unit = {
        val p = ch.pipeline()

        // Add proxy handler - it will intercept the connection to the target
        val proxy = client.proxy.get
        val proxyAddress = new InetSocketAddress(proxy.host, proxy.port)
        val handler = proxy.`type` match {
          case ProxyType.Socks =>
            proxy.credentials match {
              case Some(creds) => new Socks5ProxyHandler(proxyAddress, creds.username, creds.password)
              case None => new Socks5ProxyHandler(proxyAddress)
            }
          case ProxyType.Http =>
            proxy.credentials match {
              case Some(creds) => new HttpProxyHandler(proxyAddress, creds.username, creds.password)
              case None => new HttpProxyHandler(proxyAddress)
            }
          case _ => throw new IllegalStateException("Unexpected proxy type")
        }
        p.addFirst("proxy", handler) // Add proxy handler first

        // Add SSL handler if HTTPS (will be activated after proxy connect)
        if (isHttps) {
          val sslCtx = if (client.validateSSLCertificates) {
            SslContextBuilder.forClient().build()
          } else {
            SslContextBuilder.forClient()
              .trustManager(InsecureTrustManagerFactory.INSTANCE)
              .build()
          }
          p.addLast("ssl", sslCtx.newHandler(ch.alloc(), host, port))
        }

        // Add timeout handlers
        val timeoutSeconds = client.timeout.toSeconds.toInt
        p.addLast("readTimeout", new ReadTimeoutHandler(timeoutSeconds))
        p.addLast("writeTimeout", new WriteTimeoutHandler(timeoutSeconds))
        p.addLast("idleState", new IdleStateHandler(0, 0, timeoutSeconds))

        // HTTP codec
        p.addLast("httpCodec", new HttpClientCodec())

        // Response handler (not pooled)
        p.addLast("responseHandler", new NonPooledResponseHandler(task, client))
      }
    })

    val connectFuture = bootstrap.connect()
    connectFuture.addListener((f: ChannelFuture) => {
      if (!f.isSuccess) {
        task.success(Failure(f.cause()))
      } else {
        val channel = f.channel()
        // Create and send request - proxy handler automatically routes through proxy
        val nettyRequest = createNettyRequest(request, uri)
        channel.writeAndFlush(nettyRequest)
      }
    })
  }

  // Non-pooled response handler for proxy connections
  private class NonPooledResponseHandler(task: Completable[Try[HttpResponse]],
                                         client: HttpClient) extends ResponseHandler(task, client) {

    override def channelRead0(ctx: ChannelHandlerContext, msg: HttpObject): Unit = {
      super.channelRead0(ctx, msg)

      // If this was the last chunk, close the channel
      if (msg.isInstanceOf[LastHttpContent]) {
        ctx.close()
      }
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
      // Call super first to handle the exception (which already closes the channel)
      super.exceptionCaught(ctx, cause)
      // Channel is already closed by super.exceptionCaught, so no need to close again
    }
  }

  private def createNettyRequest(request: HttpRequest, uri: URL): NettyHttpRequest = {
    val nettyUri = uri.encoded.pathAndArgs
    val method = NettyHttpMethod.valueOf(request.method.value.toUpperCase)

    // Handle request content
    val (bodyBytes, contentType) = request.content match {
      case Some(content) =>
        content match {
          case FormDataContent(entries) =>
            val boundary = s"----SpiceBoundary${System.nanoTime()}"
            val bytes = buildMultipartFormData(entries, boundary)
            val ct = ContentType.`multipart/form-data`.withExtra("boundary", boundary)
            (bytes, Some(ct))
          case other =>
            (contentToBytes(other), Some(getContentType(other)))
        }
      case None => (Array.emptyByteArray, None)
    }

    val nettyContent = Unpooled.wrappedBuffer(bodyBytes)
    val nettyRequest = new DefaultFullHttpRequest(HttpVersion.HTTP_1_1, method, nettyUri, nettyContent)

    // Set headers
    val hostHeader = if ((uri.protocol == Protocol.Http && uri.port == 80) ||
      (uri.protocol == Protocol.Https && uri.port == 443)) {
      uri.host // Standard ports can be omitted
    } else {
      s"${uri.host}:${uri.port}" // Include port for non-standard ports
    }
    nettyRequest.headers().set(HttpHeaderNames.HOST, hostHeader)
    nettyRequest.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE)
    nettyRequest.headers().set(HttpHeaderNames.CONTENT_LENGTH, nettyContent.readableBytes())

    // Set content type if present
    contentType.foreach { ct =>
      nettyRequest.headers().set(HttpHeaderNames.CONTENT_TYPE, ct.outputString)
    }

    // Add custom headers (don't override system headers)
    request.headers.map.foreach {
      case (key, values) =>
        val lowerKey = key.toLowerCase
        if (lowerKey != "host" && lowerKey != "content-length" && lowerKey != "content-type") {
          values.foreach(value => nettyRequest.headers().add(key, value))
        }
    }

    nettyRequest
  }

  private def getContentType(content: Content): ContentType = content match {
    case c: StringContent => c.contentType
    case c: FileContent => c.contentType
    case c: BytesContent => c.contentType
    case c: JsonContent => c.contentType
    case c: URLContent => c.contentType
    case FormDataContent(_) => ContentType.`multipart/form-data`
    case _ => ContentType.`application/octet-stream`
  }

  private def contentToBytes(content: Content): Array[Byte] = content match {
    case StringContent(value, _, _) => value.getBytes(StandardCharsets.UTF_8)
    case BytesContent(array, _, _) => array
    case FileContent(file, _, _) =>
      val fis = new java.io.FileInputStream(file)
      try {
        fis.readAllBytes()
      } finally {
        fis.close()
      }
    case JsonContent(json, compact, _, _) =>
      val jsonString = if (compact) {
        JsonFormatter.Compact(json)
      } else {
        JsonFormatter.Default(json)
      }
      jsonString.getBytes(StandardCharsets.UTF_8)
    case URLContent(url, _, _) =>
      val conn = url.openConnection()
      val input = conn.getInputStream
      try {
        input.readAllBytes()
      } finally {
        Try(input.close())
      }
    case FormDataContent(entries) =>
      // This is handled in createNettyRequest now to avoid duplicate boundary generation
      throw new IllegalStateException("FormDataContent should be handled in createNettyRequest")
    case _ =>
      throw new RuntimeException(s"Unsupported request content: $content")
  }

  private def buildMultipartFormData(entries: Map[String, FormDataEntry], boundary: String): Array[Byte] = {
    val baos = new ByteArrayOutputStream()

    entries.foreach { case (key, entry) =>
      baos.write(s"--$boundary\r\n".getBytes(StandardCharsets.UTF_8))

      entry match {
        case StringEntry(value, _) =>
          baos.write(s"Content-Disposition: form-data; name=\"$key\"\r\n\r\n".getBytes(StandardCharsets.UTF_8))
          baos.write(value.getBytes(StandardCharsets.UTF_8))
        case FileEntry(fileName, file, headers) =>
          val contentType = Headers.`Content-Type`.value(headers)
            .getOrElse(ContentType.`application/octet-stream`)
          baos.write(s"Content-Disposition: form-data; name=\"$key\"; filename=\"$fileName\"\r\n".getBytes(StandardCharsets.UTF_8))
          baos.write(s"Content-Type: ${contentType.outputString}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
          val fis = new java.io.FileInputStream(file)
          try {
            fis.transferTo(baos)
          } finally {
            fis.close()
          }
      }
      baos.write("\r\n".getBytes(StandardCharsets.UTF_8))
    }

    baos.write(s"--$boundary--\r\n".getBytes(StandardCharsets.UTF_8))
    baos.toByteArray
  }

  /**
   * Response handler that returns the channel to the pool when done
   */
  private class PooledResponseHandler(task: Completable[Try[HttpResponse]],
                                      client: HttpClient,
                                      pool: SimpleChannelPool,
                                      channel: Channel,
                                      handlerName: String) extends ResponseHandler(task, client) {

    @volatile private var cleanedUp: Boolean = false

    override def channelRead0(ctx: ChannelHandlerContext, msg: HttpObject): Unit = {
      super.channelRead0(ctx, msg)

      // If this was the last chunk, clean up and return to pool
      if (msg.isInstanceOf[LastHttpContent]) {
        cleanup(ctx)
      }
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
      // Call super first to handle the exception
      super.exceptionCaught(ctx, cause)
      // Then clean up (remove handler and return to pool)
      cleanup(ctx)
    }

    private def cleanup(ctx: ChannelHandlerContext): Unit = {
      if (!cleanedUp) {
        cleanedUp = true
        // Remove this handler from the pipeline if it's still there
        try {
          if (ctx.pipeline().get(handlerName) != null) {
            ctx.pipeline().remove(handlerName)
          }
        } catch {
          case _: Throwable => // Handler already removed or pipeline closed, ignore
        }
        // Return channel to pool
        try {
          pool.release(channel)
        } catch {
          case _: Throwable => // Channel might already be closed, ignore
        }
      }
    }
  }

  // Base response handler
  private class ResponseHandler(task: Completable[Try[HttpResponse]], client: HttpClient)
    extends SimpleChannelInboundHandler[HttpObject] {

    private var statusCode: Int = 0
    private var headers: Headers = Headers.empty
    private var contentType: ContentType = ContentType.`application/octet-stream`
    private var contentLength: Option[Long] = None
    private var accumulator: ContentAccumulator = _
    @volatile private var completed: Boolean = false

    override def channelRead0(ctx: ChannelHandlerContext, msg: HttpObject): Unit = {
      msg match {
        case response: NettyHttpResponse =>
          statusCode = response.status().code()
          val headersMap = response.headers().entries().asScala
            .groupBy(_.getKey.toLowerCase)
            .view
            .mapValues(_.map(_.getValue).toList)
            .toMap
          headers = Headers(headersMap)

          contentType = Headers.`Content-Type`.value(headers)
            .getOrElse(ContentType.`application/octet-stream`)
          contentLength = Headers.`Content-Length`.value(headers)

          accumulator = if (shouldAccumulateInMemory(contentType, contentLength)) {
            new MemoryAccumulator()
          } else {
            new FileAccumulator(contentType, client.saveDirectory)
          }

        case chunk: HttpContent =>
          if (accumulator != null) {
            val content = chunk.content()
            if (content.readableBytes() > 0) {
              accumulator.accumulate(content)
            }

            if (chunk.isInstanceOf[LastHttpContent]) {
              if (!completed) {
                completed = true
                val responseContent = accumulator.toContent(contentType)
                val response = HttpResponse(
                  status = HttpStatus.byCode(statusCode),
                  headers = headers,
                  content = Some(responseContent)
                )
                task.success(Success(response))
              }
            }
          }

        case _ => // ignore
      }
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
      // Prevent exception from propagating to tail of pipeline
      if (!completed) {
        completed = true
        Option(accumulator).foreach(_.cleanup())
        task.success(Failure(cause))
      }
      // Close channel on exception
      ctx.close()
    }

    private def shouldAccumulateInMemory(contentType: ContentType, contentLength: Option[Long]): Boolean = {
      if (contentType.`type` == "text" || contentType.subType == "json") {
        return true
      }
      contentLength.exists(_ < 512000L)
    }
  }

  // Content accumulator trait and implementations
  private trait ContentAccumulator {
    def accumulate(content: ByteBuf): Unit

    def toContent(contentType: ContentType): Content

    def cleanup(): Unit
  }

  private class MemoryAccumulator extends ContentAccumulator {
    private val baos = new ByteArrayOutputStream()

    override def accumulate(content: ByteBuf): Unit = {
      val bytes = new Array[Byte](content.readableBytes())
      content.readBytes(bytes)
      baos.write(bytes)
    }

    override def toContent(contentType: ContentType): Content = {
      val bytes = baos.toByteArray
      if (contentType.`type` == "text" || contentType.subType == "json") {
        Content.string(new String(bytes, StandardCharsets.UTF_8), contentType)
      } else {
        Content.bytes(bytes, contentType)
      }
    }

    override def cleanup(): Unit = {
      Try(baos.close())
    }
  }

  private class FileAccumulator(contentType: ContentType, saveDirectory: String) extends ContentAccumulator {
    private val suffix = contentType.extension.getOrElse("bin")
    private val tempFile = File.createTempFile("spice", s".$suffix", new File(saveDirectory))
    private val fos = new FileOutputStream(tempFile)

    override def accumulate(content: ByteBuf): Unit = {
      val bytes = new Array[Byte](content.readableBytes())
      content.readBytes(bytes)
      fos.write(bytes)
    }

    override def toContent(contentType: ContentType): Content = {
      Try(fos.close())
      Content.file(tempFile, contentType)
    }

    override def cleanup(): Unit = {
      Try(fos.close())
      Try(tempFile.delete())
    }
  }

  override def webSocket(url: URL): WebSocket = new NettyHttpClientWebSocket(url, this)

  override def dispose(): Task[Unit] = Task {
    poolManager.close()
    eventLoopGroup.shutdownGracefully(0, 0, TimeUnit.MILLISECONDS).sync()
  }
}