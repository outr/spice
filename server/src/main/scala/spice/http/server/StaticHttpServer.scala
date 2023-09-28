package spice.http.server

import cats.effect.IO
import scribe.mdc.MDC
import spice.http.HttpExchange
import spice.http.server.handler.HttpHandler

trait StaticHttpServer extends HttpServer {
  protected val handler: HttpHandler

  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = handler.handle(exchange)
}