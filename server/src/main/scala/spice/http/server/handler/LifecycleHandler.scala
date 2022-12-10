package spice.http.server.handler

import cats.effect.IO
import spice.http.HttpExchange

trait LifecycleHandler extends HttpHandler {
  protected def preHandle(exchange: HttpExchange): IO[HttpExchange]

  protected def apply(exchange: HttpExchange): IO[HttpExchange]

  protected def postHandle(exchange: HttpExchange): IO[HttpExchange]

  override final def handle(exchange: HttpExchange): IO[HttpExchange] = preHandle(exchange).flatMap { e =>
    apply(e)
  }.flatMap { e =>
    postHandle(e)
  }
}