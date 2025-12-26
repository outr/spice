package spice.http.client

import scala.concurrent.duration.FiniteDuration

/**
 * Enhanced Netty connection pool
 */
case class NettyConnectionPool(maxIdleConnections: Int,
                               keepAlive: FiniteDuration,
                               poolManager: Option[NettyConnectionPoolManager] = None) extends ConnectionPool {
  override def idle: Int = 0
  override def active: Int = 0
}
