package spice.http.content

import rapid.Task
import spice.net.ContentType

case class StreamContent(stream: rapid.Stream[Byte],
                         contentType: ContentType,
                         lastModified: Long = System.currentTimeMillis(),
                         length: Long = -1L) extends Content {
  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def asString: Task[String] = stream
    .toList
    .map(_.toArray)
    .map(array => new String(array, "UTF-8"))

  override def asStream: rapid.Stream[Byte] = stream
}