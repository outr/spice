package spice.http.client

import moduload.Moduload
import spice.http.content.{Content, StringContent}

import scala.concurrent.duration.FiniteDuration

object JSHttpClientImplementation extends Moduload with HttpClientImplementation {
  override def load(): Unit = {
    scribe.info(s"Registering JSHttpClientImplementation...")
    HttpClientImplementationManager.register(_ => this)
  }

  override def error(t: Throwable): Unit = {
    scribe.error("Error while attempting to register JSHttpClientImplementation", t)
  }

  override def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool =
    JSConnectionPool(maxIdleConnections, keepAlive)

  override protected def createInstance(client: HttpClient): HttpClientInstance = new JSHttpClientInstance(client)
}
