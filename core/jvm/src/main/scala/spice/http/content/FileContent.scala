package spice.http.content

import rapid.*
import spice.net.ContentType
import spice.streamer.*
import spice.streamer.given

import java.io.{BufferedInputStream, File, InputStream, RandomAccessFile}
import scala.collection.mutable

/** A file to send back. `offset` and `count` narrow it to a window of its bytes — what a `Range` request asks for —
  * and everything that reads the content (its length, its stream, its words) reads only that window. */
case class FileContent(file: File,
                       contentType: ContentType,
                       lastModifiedOverride: Option[Long] = None,
                       offset: Long = 0L,
                       count: Option[Long] = None) extends Content {
  assert(file.isFile, s"Cannot send back ${file.getAbsolutePath} as it is a directory or does not exist!")

  /** Whether this is the file entire, rather than a window of it. */
  def whole: Boolean = offset == 0L && count.isEmpty

  override def length: Long = count.getOrElse(math.max(0L, file.length() - offset))

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModifiedOverride = Some(lastModified))

  /** The `count` bytes of this file that begin at `offset`. */
  def range(offset: Long, count: Long): FileContent = copy(offset = offset, count = Some(count))

  override def lastModified: Long = lastModifiedOverride.getOrElse(file.lastModified())

  override def toString: String =
    if (whole) s"FileContent(file: ${file.getAbsolutePath}, contentType: $contentType)"
    else s"FileContent(file: ${file.getAbsolutePath}, contentType: $contentType, offset: $offset, length: $length)"

  override def asString: Task[String] =
    if (whole) Streamer(file, new mutable.StringBuilder).map(_.toString)
    else Task {
      val bytes = new Array[Byte](math.min(length, Int.MaxValue.toLong).toInt)
      val raf = new RandomAccessFile(file, "r")
      try {
        raf.seek(offset)
        raf.readFully(bytes)
      } finally raf.close()
      new String(bytes, "UTF-8")
    }

  override def asStream: rapid.Stream[Byte] =
    if (whole) rapid.Stream.fromFile(file)
    else rapid.Stream.fromInputStream(Task(new BufferedInputStream(window)))

  /** The window as a stream of its own: it begins at `offset` and ends after `length` bytes. */
  def window: InputStream = new InputStream {
    private val raf = new RandomAccessFile(file, "r")
    private var remaining: Long = length
    raf.seek(offset)

    override def read(): Int =
      if (remaining <= 0L) -1
      else {
        val b = raf.read()
        if (b >= 0) remaining -= 1L
        b
      }

    override def read(b: Array[Byte], off: Int, len: Int): Int =
      if (remaining <= 0L) -1
      else {
        val read = raf.read(b, off, math.min(len.toLong, remaining).toInt)
        if (read > 0) remaining -= read.toLong
        read
      }

    override def available(): Int = math.min(remaining, Int.MaxValue.toLong).toInt

    override def close(): Unit = raf.close()
  }
}
