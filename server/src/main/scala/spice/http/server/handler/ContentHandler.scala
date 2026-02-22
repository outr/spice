package spice.http.server.handler

import rapid.*
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content

case class ContentHandler(content: Content, status: HttpStatus) extends HttpHandler {
  override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = {
    exchange.modify { response =>
      Task(response.copy(status = status, content = Some(content)))
    }
  }
}