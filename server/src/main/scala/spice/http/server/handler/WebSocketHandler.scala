package spice.http.server.handler

import rapid.*
import scribe.mdc.MDC
import spice.http.{HttpExchange, WebSocketListener}

trait WebSocketHandler extends HttpHandler {
  def connect(exchange: HttpExchange, listener: WebSocketListener): Task[Unit]

  override def handle(exchange: HttpExchange)
                     (using mdc: MDC): Task[HttpExchange] = exchange.withWebSocketListener().flatMap {
    case (exchange, listener) => connect(exchange, listener).map(_ => exchange)
  }
}