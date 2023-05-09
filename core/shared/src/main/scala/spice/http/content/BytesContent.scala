package spice.http.content

import cats.effect.IO
import spice.net.ContentType

case class BytesContent(value: Array[Byte], contentType: ContentType, lastModified: Long = System.currentTimeMillis()) extends Content {
  override def length: Long = value.length

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def toString: String = s"BytesContent(${value.take(100).mkString("Array(", ", ", ")")}, contentType: $contentType)"

  override def asString: String = new String(value, "UTF-8")

  override def asStream: fs2.Stream[IO, Byte] = fs2.Stream.fromIterator[IO](value.iterator, 1024)
}