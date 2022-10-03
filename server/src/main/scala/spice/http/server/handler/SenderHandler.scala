package spice.http.server.handler

import cats.effect.IO
import spice.http.{Headers, HttpExchange}
import spice.http.content.Content

class SenderHandler private(content: Content, length: Option[Long], caching: CachingManager) extends HttpHandler {
  override def handle(exchange: HttpExchange): IO[HttpExchange] = {
    SenderHandler.handle(exchange, content, length, caching)
  }
}

object SenderHandler {
  def apply(content: Content, length: Option[Long] = None, caching: CachingManager = CachingManager.Default): SenderHandler = {
    new SenderHandler(content, length, caching)
  }

  def handle(exchange: HttpExchange,
             content: Content,
             length: Option[Long] = None,
             caching: CachingManager = CachingManager.Default,
             replace: Boolean = false): IO[HttpExchange] = {
    if (exchange.response.content.nonEmpty && !replace) {
      throw new RuntimeException(s"Content already set (${exchange.response.content.get}) for HttpResponse in ${exchange.request.url} when attempting to set $content.")
    }
    val contentLength = length.getOrElse(content.length)
    exchange.modify { response =>
      IO(response.withContent(content).withHeader(Headers.`Content-Length`(contentLength)))
    }.flatMap(caching.handle)
  }
}