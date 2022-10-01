package spice.http

import spice.http.content.Content
import spice.net.ContentType

import java.io.OutputStream

abstract class IOStreamContent(val contentType: ContentType,
                               val lastModified: Long = System.currentTimeMillis(),
                               val length: Long = -1L) extends Content {
  def stream(out: OutputStream): Unit
}