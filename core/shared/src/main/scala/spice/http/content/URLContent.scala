package spice.http.content

import rapid.Task
import spice.net.ContentType
import spice.streamer.*
import spice.streamer.given

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

  override def asString: Task[String] = Streamer(url, new mutable.StringBuilder).map(_.toString)

  override def asStream: rapid.Stream[Byte] = rapid.Stream.fromInputStream(Task(url.openStream()))
}