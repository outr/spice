package spice.http.content

import java.io.OutputStream

import spice.net.ContentType

abstract class StreamContent(val contentType: ContentType,
                             val lastModified: Long = System.currentTimeMillis(),
                             val length: Long = -1L) extends Content {
  def stream(out: OutputStream): Unit
}