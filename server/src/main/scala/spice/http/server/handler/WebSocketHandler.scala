package spice.http.server.handler

import cats.effect.IO
import scribe.data.MDC
import spice.http.{HttpExchange, WebSocketListener}

trait WebSocketHandler extends HttpHandler {
  def connect(exchange: HttpExchange, listener: WebSocketListener): IO[Unit]

  override def handle(exchange: HttpExchange)
                     (implicit mdc: MDC): IO[HttpExchange] = exchange.withWebSocketListener().flatMap {
    case (exchange, listener) => connect(exchange, listener).map(_ => exchange)
  }
}