package spice.http.server.handler

import rapid.Task
import scribe.mdc.MDC
import spice.http.content.Content
import spice.http.{Headers, HttpExchange}

case class SenderHandler(content: Content,
                         length: Option[Long] = None,
                         caching: CachingManager = CachingManager.Default,
                         replace: Boolean = false) extends HttpHandler {
  override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] =
    SenderHandler.handle(exchange, content, length, caching, replace)
}

object SenderHandler {
  def handle(exchange: HttpExchange,
             content: Content,
             length: Option[Long] = None,
             caching: CachingManager = CachingManager.Default,
             replace: Boolean = false): Task[HttpExchange] = {
    if (exchange.response.content.nonEmpty && !replace) {
      throw new RuntimeException(s"Content already set (${exchange.response.content.get}) for HttpResponse in ${exchange.request.url} when attempting to set $content.")
    }
    val contentLength = length.getOrElse(content.length)
    exchange.modify { response =>
      // setHeader (replace) — `withContent` already wrote Content-Length; using `withHeader`
      // here would APPEND a duplicate, producing `Content-Length: N, N` on the wire which
      // clients (per RFC 7230 §3.3.2) treat as malformed and may strip entirely.
      Task(response.withContent(content).setHeader(Headers.`Content-Length`(contentLength)))
    }.flatMap(caching.handle)
  }
}