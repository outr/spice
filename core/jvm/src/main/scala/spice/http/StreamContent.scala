package spice.http

import cats.effect.IO
import spice.http.content.Content
import spice.net.ContentType

case class StreamContent(stream: fs2.Stream[IO, Byte],
                         contentType: ContentType,
                         lastModified: Long = System.currentTimeMillis(),
                         length: Long = -1L) extends Content {
  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def asString: String = throw new RuntimeException("StreamContent cannot be converted to String")
}