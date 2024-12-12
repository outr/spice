package spice.http.content

import spice.net.ContentType

import java.io.OutputStream

abstract class IOStreamContent(val contentType: ContentType,
                               val lastModified: Long = System.currentTimeMillis(),
                               val length: Long = -1L) extends Content {
  def stream(out: OutputStream): Unit

  override def asStream: rapid.Stream[Byte] = throw new UnsupportedOperationException("IOStreamContent cannot be represented as a stream!")
}