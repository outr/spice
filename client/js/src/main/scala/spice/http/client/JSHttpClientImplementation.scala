package spice.http.client

import spice.http.content.{Content, StringContent}

import scala.concurrent.duration.FiniteDuration

object JSHttpClientImplementation extends HttpClientImplementation {
  override def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool =
    JSConnectionPool(maxIdleConnections, keepAlive)

  override protected def createInstance(client: HttpClient): HttpClientInstance = new JSHttpClientInstance(client)
}
