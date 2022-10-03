package spice.http.server.handler

import cats.effect.IO
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content

case class ContentHandler(content: Content, status: HttpStatus) extends HttpHandler {
  override def handle(exchange: HttpExchange): IO[HttpExchange] = {
    exchange.modify { response =>
      IO(response.copy(status = status, content = Some(content)))
    }
  }
}