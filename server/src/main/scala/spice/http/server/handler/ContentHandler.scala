package spice.http.server.handler

import cats.effect.IO
import scribe.data.MDC
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content

case class ContentHandler(content: Content, status: HttpStatus) extends HttpHandler {
  override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = {
    exchange.modify { response =>
      IO(response.copy(status = status, content = Some(content)))
    }
  }
}