package spice.http.content

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import spice.net.ContentType
import spice.streamer._

import java.net.URL
import scala.collection.mutable
import scala.util.Try

case class URLContent(url: URL, contentType: ContentType, lastModifiedOverride: Option[Long] = None) extends Content {
  assert(url != null, "URL must not be null.")

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModifiedOverride = Some(lastModified))

  private lazy val (contentLength, contentModified) = {
    val connection = url.openConnection()
    try {
      connection.getContentLengthLong -> connection.getLastModified
    } finally {
      Try(connection.getInputStream.close())
    }
  }

  override def length: Long = contentLength

  override def lastModified: Long = lastModifiedOverride.getOrElse(contentModified)

  override def toString: String = s"URLContent(url: $url, contentType: $contentType)"

  override def asString: IO[String] = Streamer(url, new mutable.StringBuilder).map(_.toString)

  override def asStream: fs2.Stream[IO, Byte] = fs2.io.readInputStream[IO](IO(url.openStream()), 1024)
}