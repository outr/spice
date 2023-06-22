package spice.http.client

import cats.effect.IO
import cats.implicits.toTraverseOps
import scala.concurrent.duration.FiniteDuration

trait HttpClientImplementation {
  private var instances = Map.empty[String, HttpClientInstance]

  def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool

  final def instance(client: HttpClient): HttpClientInstance = synchronized {
    instances.get(client.instanceKey) match {
      case Some(i) => i
      case None =>
        val i = createInstance(client)
        instances += client.instanceKey -> i
        i
    }
  }

  protected def createInstance(client: HttpClient): HttpClientInstance

  def dispose(): IO[Unit] = instances.map(_._2.dispose()).toList.sequence.map { _ =>
    instances = Map.empty
  }
}