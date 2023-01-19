package spice.http.client

import cats.effect.IO
import cats.implicits.toTraverseOps
import spice.http.content.Content

import scala.concurrent.duration.FiniteDuration

trait HttpClientImplementation {
  private var instances = Set.empty[HttpClientInstance]

  def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool

  final def instance(client: HttpClient): HttpClientInstance = synchronized {
    val i = createInstance(client)
    instances += i
    i
  }

  protected def createInstance(client: HttpClient): HttpClientInstance

  def content2String(content: Content): String

  def dispose(): IO[Unit] = instances.map(_.dispose()).toList.sequence.map(_ => ())
}