package spice.http.content

import fabric.Json
import fabric.io.JsonFormatter
import fabric.rw._
import rapid.Task
import spice.net.ContentType

import scala.collection.compat.immutable.ArraySeq

case class JsonContent(json: Json,
                       compact: Boolean = true,
                       contentType: ContentType = ContentType.`application/json`,
                       lastModified: Long = System.currentTimeMillis()) extends Content {
  lazy val value: String = if (compact) JsonFormatter.Compact(json) else JsonFormatter.Default(json)

  override def length: Long = value.getBytes("UTF-8").length

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def asString: Task[String] = Task.pure(value)

  override def asStream: rapid.Stream[Byte] =
    rapid.Stream.emits(ArraySeq.unsafeWrapArray(value.getBytes("UTF-8")))
}

object JsonContent {
  def from[T: RW](value: T, compact: Boolean = true): JsonContent = JsonContent(value.json, compact)
}