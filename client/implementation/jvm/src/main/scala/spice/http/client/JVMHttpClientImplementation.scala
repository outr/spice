package spice.http.client

import moduload.Moduload

import scala.concurrent.duration.FiniteDuration

object JVMHttpClientImplementation extends Moduload with HttpClientImplementation {
  override def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool = ???

  override protected def createInstance(client: HttpClient): HttpClientInstance = new JVMHttpClientInstance(client)

  override def load(): Unit = {
    HttpClientImplementationManager.register(_ => this)
  }

  override def error(t: Throwable): Unit = {
    scribe.error("Error while attempting to register JVMClientImplementation", t)
  }
}