package spice.http.server

import cats.effect.IO
import spice.http.HttpExchange

trait ErrorHandler {
  def handle(exchange: HttpExchange, t: Option[Throwable]): IO[HttpExchange]
}
