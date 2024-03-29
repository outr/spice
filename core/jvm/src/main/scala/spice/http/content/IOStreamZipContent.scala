package spice.http.content

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import spice.net.ContentType
import spice.streamer._

import java.io.OutputStream
import java.util.zip.{ZipEntry, ZipOutputStream}

class IOStreamZipContent(entries: List[ZipFileEntry],
                         lastModified: Long = System.currentTimeMillis(),
                         length: Long = -1,
                         contentType: ContentType = ContentType.`application/zip`) extends IOStreamContent(contentType, lastModified, length) {
  override def withContentType(contentType: ContentType): Content = {
    new IOStreamZipContent(entries, lastModified, length, contentType)
  }

  override def withLastModified(lastModified: Long): Content = {
    new IOStreamZipContent(entries, lastModified, length, contentType)
  }

  override def stream(out: OutputStream): Unit = {
    val zos = new ZipOutputStream(out)
    entries.foreach { e =>
      val entry = new ZipEntry(e.path)
      zos.putNextEntry(entry)
      Streamer(e.file, zos, closeOnComplete = false).unsafeRunSync()
      zos.closeEntry()
    }
    zos.flush()
    zos.close()
  }

  override def asString: IO[String] = IO(entries.mkString(", "))
}