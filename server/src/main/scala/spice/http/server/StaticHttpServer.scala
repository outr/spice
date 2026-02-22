package spice.http.server

import rapid.Task
import scribe.mdc.MDC
import spice.http.HttpExchange
import spice.http.server.handler.HttpHandler

trait StaticHttpServer extends HttpServer {
  protected val handler: HttpHandler

  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = handler.handle(exchange)
}