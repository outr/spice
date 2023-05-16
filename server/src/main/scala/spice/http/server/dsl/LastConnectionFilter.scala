package spice.http.server.dsl

import cats.effect.IO
import scribe.data.MDC
import spice.http.HttpExchange

case class LastConnectionFilter(filters: ConnectionFilter*) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[FilterResponse] = IO {
    last(exchange, filters: _*)
    FilterResponse.Continue(exchange)
  }
}