package spice.http.server.dsl

import cats.effect.IO
import spice.http.HttpExchange

case class LastConnectionFilter(filters: ConnectionFilter*) extends ConnectionFilter {
  override def apply(exchange: HttpExchange): IO[FilterResponse] = IO {
    last(exchange, filters: _*)
    FilterResponse.Continue(exchange)
  }
}