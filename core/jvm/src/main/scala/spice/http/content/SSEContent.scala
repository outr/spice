package spice.http.content

import rapid.Task
import spice.net.ContentType

/** Server-Sent Events content. Each string in the stream is an SSE event
  * (should be formatted as "data: ...\n\n"). The response is sent with
  * `text/event-stream` content type and appropriate headers for streaming. */
case class SSEContent(events: rapid.Stream[String],
                      lastModified: Long = System.currentTimeMillis(),
                      length: Long = -1L) extends Content {
  override def contentType: ContentType = ContentType.parse("text/event-stream")
  override def withContentType(contentType: ContentType): Content = this // SSE always uses text/event-stream
  override def withLastModified(lastModified: Long): Content = copy(lastModified = lastModified)

  override def asString: Task[String] = events.toList.map(_.mkString("\n"))

  override def asStream: rapid.Stream[Byte] = events
    .map(event => (event + "\n").getBytes("UTF-8"))
    .flatMap(bytes => rapid.Stream.emits(bytes.toList))
}
