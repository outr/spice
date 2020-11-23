package spice.http.content

import java.io.OutputStream
import java.util.zip.{ZipEntry, ZipOutputStream}

import spice.net.ContentType

class StreamZipContent(entries: List[ZipFileEntry],
                       lastModified: Long = System.currentTimeMillis(),
                       length: Long = -1,
                       contentType: ContentType = ContentType.`application/zip`) extends StreamContent(contentType, lastModified, length) {
  override def withContentType(contentType: ContentType): Content = {
    new StreamZipContent(entries, lastModified, length, contentType)
  }

  override def withLastModified(lastModified: Long): Content = {
    new StreamZipContent(entries, lastModified, length, contentType)
  }

  override def stream(out: OutputStream): Unit = {
    val zos = new ZipOutputStream(out)
    entries.foreach { e =>
      val entry = new ZipEntry(e.path)
      zos.putNextEntry(entry)
      IO.stream(e.file, zos, closeOnComplete = false)
      zos.closeEntry()
    }
    zos.flush()
    zos.close()
  }

  override def asString: String = entries.mkString(", ")
}