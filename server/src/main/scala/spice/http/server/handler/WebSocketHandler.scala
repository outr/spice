package spice.http.server.handler

import cats.effect.IO
import spice.http.{HttpExchange, WebSocketListener}

trait WebSocketHandler extends HttpHandler {
  def connect(exchange: HttpExchange, listener: WebSocketListener): IO[Unit]

  override def handle(exchange: HttpExchange): IO[HttpExchange] = exchange.withWebSocketListener().flatMap {
    case (exchange, listener) => connect(exchange, listener).map(_ => exchange)
  }
}