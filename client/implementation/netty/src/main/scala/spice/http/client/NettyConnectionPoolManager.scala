package spice.http.client

import io.netty.channel.pool._
import io.netty.bootstrap.Bootstrap
import io.netty.channel._
import io.netty.channel.MultiThreadIoEventLoopGroup
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.pool.FixedChannelPool.AcquireTimeoutAction
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.http._
import io.netty.handler.proxy.{HttpProxyHandler, Socks5ProxyHandler}
import io.netty.handler.ssl.SslContextBuilder
import io.netty.handler.ssl.util.InsecureTrustManagerFactory
import io.netty.handler.timeout.{IdleStateHandler, ReadTimeoutHandler, WriteTimeoutHandler}
import io.netty.util.concurrent.Future

import java.net.InetSocketAddress
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.duration.FiniteDuration

/**
 * Connection pool manager for Netty HTTP client
 */
class NettyConnectionPoolManager(
                                  eventLoopGroup: MultiThreadIoEventLoopGroup,
                                  client: HttpClient,
                                  maxConnections: Int,
                                  keepAlive: FiniteDuration
                                ) {

  private val pools = new ConcurrentHashMap[PoolKey, SimpleChannelPool]()

  case class PoolKey(host: String, port: Int, isHttps: Boolean, proxyInfo: Option[String])

  def getPool(host: String, port: Int, isHttps: Boolean): SimpleChannelPool = {
    val proxyInfo = client.proxy.map(p => s"${p.host}:${p.port}")
    val key = PoolKey(host, port, isHttps, proxyInfo)
    pools.computeIfAbsent(key, k => createPool(k))
  }

  private def createPool(key: PoolKey): SimpleChannelPool = {
    val bootstrap = new Bootstrap()
      .group(eventLoopGroup)
      .channel(classOf[NioSocketChannel])
      .option[Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, client.timeout.toMillis.toInt)

    // When using a proxy, connect to the proxy server instead of the target
    client.proxy match {
      case Some(proxy) if proxy.`type` != ProxyType.Direct =>
        bootstrap.remoteAddress(new InetSocketAddress(proxy.host, proxy.port))
      case _ =>
        bootstrap.remoteAddress(new InetSocketAddress(key.host, key.port))
    }

    new FixedChannelPool(
      bootstrap,
      new ChannelPoolHandler {
        override def channelCreated(ch: Channel): Unit = {
          val p = ch.pipeline()

          // Add proxy handler FIRST (before SSL)
          client.proxy match {
            case Some(proxy) if proxy.`type` == ProxyType.Socks =>
              val proxyAddress = new InetSocketAddress(proxy.host, proxy.port)
              val handler = proxy.credentials match {
                case Some(creds) => new Socks5ProxyHandler(proxyAddress, creds.username, creds.password)
                case None => new Socks5ProxyHandler(proxyAddress)
              }
              p.addLast("socks", handler)
            case Some(proxy) if proxy.`type` == ProxyType.Http =>
              val proxyAddress = new InetSocketAddress(proxy.host, proxy.port)
              val handler = proxy.credentials match {
                case Some(creds) => new HttpProxyHandler(proxyAddress, creds.username, creds.password)
                case None => new HttpProxyHandler(proxyAddress)
              }
              p.addLast("http-proxy", handler)
            case _ => // No proxy or Direct proxy
          }

          // Add SSL handler if HTTPS (AFTER proxy)
          if (key.isHttps) {
            val sslCtx = if (client.validateSSLCertificates) {
              SslContextBuilder.forClient().build()
            } else {
              SslContextBuilder.forClient()
                .trustManager(InsecureTrustManagerFactory.INSTANCE)
                .build()
            }
            p.addLast("ssl", sslCtx.newHandler(ch.alloc(), key.host, key.port))
          }

          // Add timeout handlers (removed readTimeout based on earlier discussion)
          val timeoutSeconds = client.timeout.toSeconds.toInt
          p.addLast("writeTimeout", new WriteTimeoutHandler(timeoutSeconds))
          p.addLast("idleState", new IdleStateHandler(
            keepAlive.toSeconds.toInt,
            keepAlive.toSeconds.toInt,
            keepAlive.toSeconds.toInt
          ))

          // HTTP codec
          p.addLast("httpCodec", new HttpClientCodec())
        }

        override def channelAcquired(ch: Channel): Unit = {
          // Reset pipeline for reuse
        }

        override def channelReleased(ch: Channel): Unit = {
          // Clear any pending data
          ch.flush()
        }
      },
      ChannelHealthChecker.ACTIVE,
      AcquireTimeoutAction.FAIL,
      client.timeout.toMillis,
      maxConnections,
      maxConnections,
      true // release channel on close
    )
  }

  def close(): Unit = {
    import scala.jdk.CollectionConverters._
    pools.values().asScala.foreach { pool =>
      pool.close()
    }
    pools.clear()
  }
}