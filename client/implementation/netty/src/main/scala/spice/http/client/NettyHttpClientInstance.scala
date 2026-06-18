package spice.http.client

import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.*
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.pool.SimpleChannelPool
import io.netty.handler.proxy.{HttpProxyHandler, Socks5ProxyHandler}
import io.netty.handler.codec.http.{DefaultFullHttpRequest, HttpClientCodec, HttpContent, HttpContentDecompressor, HttpHeaderNames, HttpHeaderValues, HttpHeaders, HttpObject, HttpVersion, LastHttpContent, HttpMethod as NettyHttpMethod, HttpRequest as NettyHttpRequest, HttpResponse as NettyHttpResponse}
import io.netty.util.concurrent.{Future as NettyFuture}
import rapid.Task
import spice.http.{HttpRequest, HttpResponse, *}
import spice.http.content.*
import spice.net.{ContentType, Protocol, URL}
import fabric.io.JsonFormatter
import io.netty.bootstrap.Bootstrap
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.handler.timeout.{IdleStateEvent, IdleStateHandler, ReadTimeoutException, ReadTimeoutHandler, WriteTimeoutHandler}
import rapid.task.Completable
import scribe.*
import spice.http.content.FormDataEntry.{FileEntry, StringEntry}

import java.io.{ByteArrayOutputStream, File, FileOutputStream}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
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
          bindActiveRequestTask(channel, task)
          // Create response handler with a unique name to allow safe removal
          val handlerName = s"responseHandler-${System.nanoTime()}"
          val responseHandler = new PooledResponseHandler(task, client, pool, channel, handlerName)
          val pipeline = channel.pipeline()
          if (pipeline.get("exceptionHandler") != null) {
            pipeline.addBefore("exceptionHandler", handlerName, responseHandler)
          } else {
            pipeline.addLast(handlerName, responseHandler)
          }

          // Create and send request
          val nettyRequest = createNettyRequest(request, uri)
          channel.writeAndFlush(nettyRequest).addListener((wf: ChannelFuture) => {
            if (!wf.isSuccess) {
              responseHandler.failOnWrite(wf.cause())
              if (channel.isOpen) {
                channel.close()
              }
            }
          })
        } catch {
          case t: Throwable =>
            clearActiveRequestTask(channel)
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
        p.addLast("httpDecompressor", new HttpContentDecompressor(0))

        // Response handler (not pooled)
        p.addLast("responseHandler", new NonPooledResponseHandler(task, client))
        
        // Add permanent exception handler as a safety net to catch any exceptions
        // that might not be handled by the response handler (e.g., during channel close)
        p.addLast("exceptionHandler", new ChannelDuplexHandler {
          override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
            cause match {
              case _: ReadTimeoutException =>
                if (ctx.channel().isOpen) {
                  ctx.close()
                }
              case _ =>
                if (ctx.channel().isOpen) {
                  scribe.debug(s"Exception caught in pipeline: ${cause.getMessage}")
                  ctx.close()
                }
            }
          }
        })
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
        channel.writeAndFlush(nettyRequest).addListener((wf: ChannelFuture) => {
          if (!wf.isSuccess) {
            val cause = Option(wf.cause()).getOrElse(new RuntimeException("Failed writing HTTP request"))
            task.success(Failure(cause))
            if (channel.isOpen) {
              channel.close()
            }
          }
        })
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

    private val cleanedUp: AtomicBoolean = new AtomicBoolean(false)

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

    override def channelInactive(ctx: ChannelHandlerContext): Unit = {
      super.channelInactive(ctx)
      cleanup(ctx)
    }

    def cleanup(): Unit = {
      if (channel.isRegistered) {
        cleanup(channel.pipeline().context(this))
      } else {
        releaseChannel()
      }
    }

    def failOnWrite(cause: Throwable): Unit = {
      failRequest(cause)
      cleanup()
    }

    private def cleanup(ctx: ChannelHandlerContext): Unit = {
      if (cleanedUp.compareAndSet(false, true)) {
        if (ctx != null) {
          try {
            if (ctx.pipeline().get(handlerName) != null) {
              ctx.pipeline().remove(handlerName)
            }
          } catch {
            case _: Throwable => // Handler already removed or pipeline closed, ignore
          }
        }
        releaseChannel()
      }
    }

    private def releaseChannel(): Unit = {
      clearActiveRequestTask(channel)
      try {
        pool.release(channel)
      } catch {
        case _: Throwable => // Channel might already be closed, ignore
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
    private var accumulator: ContentAccumulator = scala.compiletime.uninitialized
    private val completed: AtomicBoolean = new AtomicBoolean(false)

    protected def failRequest(cause: Throwable): Unit = {
      if (completed.compareAndSet(false, true)) {
        Option(accumulator).foreach(_.cleanup())
        task.success(Failure(cause))
      }
    }

    private def completeSuccess(response: HttpResponse): Unit = {
      if (completed.compareAndSet(false, true)) {
        task.success(Success(response))
      }
    }

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

          // Ignore informational responses (1xx) and wait for final response headers.
          if (statusCode >= 100 && statusCode < 200) {
            accumulator = null
          } else {
            accumulator = if (shouldAccumulateInMemory(contentType, contentLength)) {
              new MemoryAccumulator()
            } else {
              new FileAccumulator(contentType, client.saveDirectory)
            }
          }

        case chunk: HttpContent =>
          if (accumulator != null) {
            val content = chunk.content()
            if (content.readableBytes() > 0) {
              accumulator.accumulate(content)
            }

            if (chunk.isInstanceOf[LastHttpContent]) {
              val responseContent = accumulator.toContent(contentType)
              val response = HttpResponse(
                status = HttpStatus.byCode(statusCode),
                headers = headers,
                content = Some(responseContent)
              )
              completeSuccess(response)
            }
          }

        case _ => // ignore
      }
    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = {
      failRequest(cause)
      // Close channel on exception
      ctx.close()
    }

    override def userEventTriggered(ctx: ChannelHandlerContext, evt: Any): Unit = {
      evt match {
        case _: IdleStateEvent =>
          failRequest(new TimeoutException("Connection became idle before response completed"))
          ctx.close()
        case _ =>
          super.userEventTriggered(ctx, evt)
      }
    }

    override def channelInactive(ctx: ChannelHandlerContext): Unit = {
      failRequest(new RuntimeException("Channel became inactive before response completed"))
      super.channelInactive(ctx)
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

  /** Stream response body line-by-line via Netty. Lines are delivered incrementally
    * as HTTP chunks arrive — not buffered. Used for SSE and streaming APIs. */
  override def sendStream(request: HttpRequest): Task[rapid.Stream[String]] =
    sendStreamHandle(request).map(_.stream)

  /** Stream the response body line-by-line via Netty, paired with a [[StreamHandle]] whose `cancel`
    * task closes the underlying channel — aborting the in-flight streaming call. */
  override def sendStreamHandle(request: HttpRequest): Task[StreamHandle[String]] = {
    val streamReady = Task.completable[StreamHandle[String]]
    val uri = request.url
    val host = uri.host
    val port = uri.port
    val isHttps = uri.protocol == Protocol.Https
    val pool = poolManager.getPool(host, port, isHttps)

    pool.acquire().addListener((future: NettyFuture[Channel]) => {
      if (!future.isSuccess) {
        streamReady.failure(future.cause())
      } else {
        val channel = future.getNow
        // A channel with streaming idle handlers must not return to the pool.
        val streamReconfigured = client.streamingTimeout.nonEmpty

        // Return the acquired channel to its pool EXACTLY ONCE, on every
        // terminal path (clean end, error, cancel, write-failure, inactive).
        // `FixedChannelPool` only decrements its acquiredChannelCount on
        // `release` — closing a channel without releasing leaks the slot, so
        // a long-lived session of streaming calls (which always close their
        // reconfigured channel) eventually exhausts the per-host pool and
        // every further acquire fails with a timeout. A streaming-reconfigured
        // channel carries streaming idle handlers and must not be re-pooled,
        // so it's closed first; the release then fails the pool's health
        // check and discards it while still decrementing the count. `close`
        // forces the same discard on the error / cancel paths. Defined above
        // the try so the outer catch can also release.
        val channelReleased = new AtomicBoolean(false)
        def returnChannel(close: Boolean): Unit =
          if (channelReleased.compareAndSet(false, true)) {
            try {
              if ((close || streamReconfigured) && channel.isOpen) {
                try { channel.close() } catch { case _: Throwable => }
              }
              pool.release(channel)
            } catch { case _: Throwable => }
          }

        try {
          val handlerName = s"streamHandler-${System.nanoTime()}"
          val lineQueue = new java.util.concurrent.LinkedBlockingQueue[Either[Throwable, Option[String]]]()
          val lineBuffer = new StringBuilder
          val pollIntervalMillis = 1000L

          // Completable surfacing the response headers to the returned
          // StreamHandle. Fires once when the upstream response arrives
          // (success OR error path); callers that need rate-limit /
          // cache-control / retry-after metadata await it.
          val responseHeadersFut = Task.completable[spice.http.Headers]

          val handler = new SimpleChannelInboundHandler[HttpObject] {
            private var headersSeen = false
            private var errorCode: Int = 0
            private val errorBody = new StringBuilder
            private var errorHeaders: spice.http.Headers = spice.http.Headers.empty

            override def channelRead0(ctx: ChannelHandlerContext, msg: HttpObject): Unit = msg match {
              case response: NettyHttpResponse =>
                headersSeen = true
                val code = response.status().code()
                // Surface response headers for every path (2xx + 4xx +
                // 5xx) so the StreamHandle's responseHeaders Task always
                // completes once the upstream replies.
                val nettyHeaders = response.headers()
                val headersMap: Map[String, List[String]] = nettyHeaders.names().asScala.toList.map { name =>
                  name -> nettyHeaders.getAll(name).asScala.toList
                }.toMap
                val parsedHeaders = spice.http.Headers(headersMap)
                responseHeadersFut.success(parsedHeaders)
                if (code >= 400) {
                  errorCode = code // defer error until body is read
                  errorHeaders = parsedHeaders
                }

              case chunk: HttpContent if headersSeen =>
                val content = chunk.content()
                if (content.readableBytes() > 0) {
                  val bytes = new Array[Byte](content.readableBytes())
                  content.readBytes(bytes)
                  val text = new String(bytes, StandardCharsets.UTF_8)
                  if (errorCode > 0) {
                    // Buffer error response body instead of emitting lines
                    errorBody.append(text)
                  } else {
                    for (c <- text) {
                      if (c == '\n') {
                        val line = lineBuffer.toString.stripSuffix("\r")
                        lineBuffer.clear()
                        lineQueue.offer(Right(Some(line)))
                      } else {
                        lineBuffer.append(c)
                      }
                    }
                  }
                }
                if (chunk.isInstanceOf[LastHttpContent]) {
                  if (errorCode > 0) {
                    // Emit error with full response body and captured headers.
                    val body = errorBody.toString.take(2000)
                    lineQueue.offer(Left(new StreamingHttpFailedException(
                      status  = errorCode,
                      headers = errorHeaders,
                      body    = body
                    )))
                    lineQueue.offer(Right(None))
                  } else {
                    if (lineBuffer.nonEmpty) {
                      lineQueue.offer(Right(Some(lineBuffer.toString)))
                      lineBuffer.clear()
                    }
                    lineQueue.offer(Right(None)) // End sentinel
                  }
                  try {
                    val p = ctx.pipeline()
                    if (p.get(handlerName) != null) p.remove(handlerName)
                  } catch { case _: Throwable => }
                  clearActiveRequestTask(channel)
                  returnChannel(close = false)
                }

              case _ =>
            }

            override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit = cause match {
              // Under a streaming budget a read-idle is a truncation, not an
              // error: end cleanly and let the consumer classify it.
              case _: ReadTimeoutException if streamReconfigured =>
                lineQueue.offer(Right(None))
                clearActiveRequestTask(channel)
              case _ =>
                lineQueue.offer(Left(cause))
                clearActiveRequestTask(channel)
                returnChannel(close = true)
            }

            override def channelInactive(ctx: ChannelHandlerContext): Unit = {
              lineQueue.offer(Right(None))
              clearActiveRequestTask(channel)
              returnChannel(close = false)
              super.channelInactive(ctx)
            }
          }

          bindActiveRequestTask(channel, Task.completable)
          val pipeline = channel.pipeline()
          if (pipeline.get("exceptionHandler") != null) {
            pipeline.addBefore("exceptionHandler", handlerName, handler)
          } else {
            pipeline.addLast(handlerName, handler)
          }
          // Streaming may pause mid-response; use the streaming budget for the
          // channel's idle handlers so a live-but-quiet socket isn't cut.
          client.streamingTimeout.foreach { st =>
            val secs = math.max(1, st.toSeconds.toInt)
            try {
              if (pipeline.get("readTimeout") != null) pipeline.replace("readTimeout", "readTimeout", new ReadTimeoutHandler(secs))
              if (pipeline.get("idleState") != null) pipeline.replace("idleState", "idleState", new IdleStateHandler(0, 0, secs))
            } catch { case _: Throwable => }
          }

          val cancelled = new AtomicBoolean(false)
          val cancelChannel = Task {
            if (cancelled.compareAndSet(false, true)) {
              // A queued sentinel terminates the consumer if a poll is in flight when cancel runs.
              lineQueue.offer(Right(None))
              clearActiveRequestTask(channel)
              returnChannel(close = true)
            }
          }.unit
          val pull = rapid.Pull.fromFunction[String](
            pullF = () => {
              // Keep waiting while the socket is open; terminate only on a
              // sentinel, error, cancellation, or close — not on idle alone.
              @scala.annotation.tailrec
              def next(): rapid.Step[String] = {
                if (cancelled.get()) rapid.Step.Stop
                else lineQueue.poll(pollIntervalMillis, java.util.concurrent.TimeUnit.MILLISECONDS) match {
                  case null              => if (channel.isOpen) next() else rapid.Step.Stop
                  case Left(err)         => throw err
                  case Right(None)       => rapid.Step.Stop
                  case Right(Some(line)) => rapid.Step.Emit(line)
                }
              }
              next()
            }
          )
          streamReady.success(StreamHandle(
            stream          = rapid.Stream(Task.pure(pull)),
            cancel          = cancelChannel,
            responseHeaders = responseHeadersFut
          ))

          val nettyRequest = createNettyRequest(request, uri)
          channel.writeAndFlush(nettyRequest).addListener((wf: ChannelFuture) => {
            if (!wf.isSuccess) {
              lineQueue.offer(Left(Option(wf.cause()).getOrElse(new RuntimeException("Write failed"))))
              clearActiveRequestTask(channel)
              returnChannel(close = true)
            }
          })
        } catch {
          case t: Throwable =>
            clearActiveRequestTask(channel)
            returnChannel(close = true)
            streamReady.failure(t)
        }
      }
    })

    streamReady
  }

  override def webSocket(url: URL): WebSocket = new NettyHttpClientWebSocket(url, this)

  override def dispose(): Task[Unit] = Task {
    poolManager.close()
    eventLoopGroup.shutdownGracefully(0, 0, TimeUnit.MILLISECONDS).sync()
  }

  private def bindActiveRequestTask(channel: Channel, task: Completable[Try[HttpResponse]]): Unit = {
    val previous = channel.attr(NettyConnectionPoolManager.ActiveRequestTaskKey).getAndSet(task)
    if (previous != null && (previous ne task)) {
      // This should never happen for a pooled channel; fail stale task defensively.
      previous.success(Failure(new IllegalStateException("Previous pooled request task was still attached to channel.")))
    }
  }

  private def clearActiveRequestTask(channel: Channel): Unit = {
    channel.attr(NettyConnectionPoolManager.ActiveRequestTaskKey).set(null)
  }
}