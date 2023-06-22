package spice.http.client

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.toTraverseOps
import spice.http.content.{BytesContent, Content, FileContent, StringContent}
import spice.streamer.Streamer

import scala.collection.mutable
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

  def content2String(content: Content): String = content match {
    case c: StringContent => c.value
    case c: BytesContent => String.valueOf(c.value)
    case c: FileContent => Streamer(c.file, new mutable.StringBuilder).unsafeRunSync().toString
    case _ => throw new RuntimeException(s"$content not supported")
  }

  def dispose(): IO[Unit] = instances.map(_._2.dispose()).toList.sequence.map { _ =>
    instances = Map.empty
  }
}