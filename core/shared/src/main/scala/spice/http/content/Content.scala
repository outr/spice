package spice.http.content

import cats.effect.IO
import spice.net.ContentType

trait Content {
  def length: Long
  def lastModified: Long
  def contentType: ContentType

  def withContentType(contentType: ContentType): Content
  def withLastModified(lastModified: Long): Content
  def asString: IO[String]
  def asStream: fs2.Stream[IO, Byte]
}

object Content extends SharedContentHelpers with ContentHelpers {
  case object none extends Content {
    override def length: Long = -1L
    override def lastModified: Long = -1L
    override def contentType: ContentType = ContentType.`text/plain`
    override def withContentType(contentType: ContentType): Content = this
    override def withLastModified(lastModified: Long): Content = this
    override def asString: IO[String] = IO.pure("none")
    override def asStream: fs2.Stream[IO, Byte] = fs2.Stream.empty
  }
}

