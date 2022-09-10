package spice.http.client

import scala.concurrent.duration.FiniteDuration

object ClientPlatform {
  def createPool(maxIdleConnections: Int = ConnectionPool.maxIdleConnections,
                 keepAlive: FiniteDuration = ConnectionPool.keepAlive): ConnectionPool = {
    JVMConnectionPool(maxIdleConnections, keepAlive)
  }

  def defaultSaveDirectory: String = System.getProperty("java.io.tmpdir")
}