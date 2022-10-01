package spice.http.client

import scala.concurrent.duration._

import reactify._

trait ConnectionPool {
  def maxIdleConnections: Int
  def keepAlive: FiniteDuration

  def idle: Int
  def active: Int
  def total: Int = idle + active
}

object ConnectionPool {
  var maxIdleConnections: Int = 100
  var keepAlive: FiniteDuration = 5.minutes

  def apply(maxIdleConnections: Int = maxIdleConnections,
            keepAlive: FiniteDuration = keepAlive,
            config: HttpClientConfig = HttpClientConfig.default): ConnectionPool = {
    HttpClientImplementationManager(config).connectionPool(maxIdleConnections, keepAlive)
  }
}