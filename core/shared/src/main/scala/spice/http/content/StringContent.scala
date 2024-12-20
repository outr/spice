package spice.http.content

import rapid.Task
import spice.net.ContentType

case class StringContent(value: String, contentType: ContentType, lastModified: Long = System.currentTimeMillis()) extends Content {
  override def length: Long = value.getBytes("UTF-8").length

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def toString: String = s"StringContent(${value.take(100)}, contentType: $contentType)"

  override def asString: Task[String] = Task.pure(value)

  override def asStream: rapid.Stream[Byte] = new rapid.Stream(Task(value.getBytes("UTF-8").iterator))
}