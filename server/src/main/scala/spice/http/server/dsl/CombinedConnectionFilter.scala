package spice.http.server.dsl

import cats.effect.IO
import scribe.mdc.MDC
import spice.http.HttpExchange

case class CombinedConnectionFilter(first: ConnectionFilter, second: ConnectionFilter) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[FilterResponse] = {
    first(exchange).flatMap {
      case FilterResponse.Continue(c) => second.apply(c)
      case stop: FilterResponse.Stop => IO.pure(stop)
    }
  }
}