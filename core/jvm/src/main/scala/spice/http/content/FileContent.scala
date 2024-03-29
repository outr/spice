package spice.http.content

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import spice.net.ContentType
import spice.streamer._

import java.io.File
import scala.collection.mutable

case class FileContent(file: File, contentType: ContentType, lastModifiedOverride: Option[Long] = None) extends Content {
  assert(file.isFile, s"Cannot send back ${file.getAbsolutePath} as it is a directory or does not exist!")

  override def length: Long = file.length()

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModifiedOverride = Some(lastModified))

  override def lastModified: Long = lastModifiedOverride.getOrElse(file.lastModified())

  override def toString: String = s"FileContent(file: ${file.getAbsolutePath}, contentType: $contentType)"

  override def asString: IO[String] = Streamer(file, new mutable.StringBuilder).map(_.toString)

  override def asStream: fs2.Stream[IO, Byte] = Files[IO].readAll(Path.fromNioPath(file.toPath))
}