package spice.http.server

import cats.effect.IO
import spice.http.HttpExchange
import spice.http.server.handler.HttpHandler

trait StaticHttpServer extends HttpServer {
  protected val handler: HttpHandler

  override def handle(exchange: HttpExchange): IO[HttpExchange] = handler.handle(exchange)
}