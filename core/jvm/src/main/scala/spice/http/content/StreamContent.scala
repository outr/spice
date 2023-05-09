package spice.http.content

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import spice.net.ContentType

case class StreamContent(stream: fs2.Stream[IO, Byte],
                         contentType: ContentType,
                         lastModified: Long = System.currentTimeMillis(),
                         length: Long = -1L) extends Content {
  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def asString: String = stream
    .compile
    .toList
    .map(_.toArray)
    .map(array => new String(array, "UTF-8"))
    .unsafeRunSync()

  override def asStream: fs2.Stream[IO, Byte] = stream
}