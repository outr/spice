package spice.http.content

import cats.effect.IO
import fabric.Json
import fabric.io.JsonFormatter
import fabric.rw._
import spice.net.ContentType

case class JsonContent(json: Json,
                       compact: Boolean = true,
                       contentType: ContentType = ContentType.`application/json`,
                       lastModified: Long = System.currentTimeMillis()) extends Content {
  lazy val value: String = if (compact) JsonFormatter.Compact(json) else JsonFormatter.Default(json)

  override def length: Long = value.getBytes("UTF-8").length

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def asString: String = value

  override def asStream: fs2.Stream[IO, Byte] =
    fs2.Stream.fromIterator[IO](asString.getBytes("UTF-8").iterator, 1024)
}

object JsonContent {
  def from[T: RW](value: T, compact: Boolean = true): JsonContent = JsonContent(value.json, compact)
}