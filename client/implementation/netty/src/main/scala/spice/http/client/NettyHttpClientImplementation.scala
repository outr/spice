package spice.http.client

import moduload.Moduload
import scala.concurrent.duration.FiniteDuration

object NettyHttpClientImplementation extends Moduload with HttpClientImplementation {
  override def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool = new ConnectionPool {
    override def maxIdleConnections: Int = 0
    override def keepAlive: FiniteDuration = keepAlive
    override def idle: Int = 0
    override def active: Int = 0
  }

  override protected def createInstance(client: HttpClient): HttpClientInstance =
    new NettyHttpClientInstance(client)

  override def load(): Unit = {
    HttpClientImplementationManager.register(_ => this)
  }

  override def error(t: Throwable): Unit = {
    scribe.error("Error while registering NettyHttpClientImplementation", t)
  }
}