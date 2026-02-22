package spice.http.client

import scala.concurrent.duration.*

import reactify.*

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

  def apply(client: HttpClient,
            maxIdleConnections: Int = maxIdleConnections,
            keepAlive: FiniteDuration = keepAlive): ConnectionPool = {
    client.implementation.connectionPool(maxIdleConnections, keepAlive)
  }
}