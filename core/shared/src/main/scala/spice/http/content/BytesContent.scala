package spice.http.content

import rapid.Task
import spice.net.ContentType

import scala.collection.compat.immutable.ArraySeq

case class BytesContent(value: Array[Byte], contentType: ContentType, lastModified: Long = System.currentTimeMillis()) extends Content {
  override def length: Long = value.length

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def toString: String = s"BytesContent(${value.take(100).mkString("Array(", ", ", ")")}, contentType: $contentType)"

  override def asString: Task[String] = Task.pure(new String(value, "UTF-8"))

  override def asStream: rapid.Stream[Byte] = rapid.Stream.emits(ArraySeq.unsafeWrapArray(value))
}