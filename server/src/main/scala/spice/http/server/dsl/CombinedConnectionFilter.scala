package spice.http.server.dsl

import cats.effect.IO
import spice.http.HttpExchange

case class CombinedConnectionFilter(first: ConnectionFilter, second: ConnectionFilter) extends ConnectionFilter {
  override def apply(exchange: HttpExchange): IO[FilterResponse] = {
    first.apply(exchange).flatMap {
      case FilterResponse.Continue(c) => second.apply(c)
      case stop: FilterResponse.Stop => IO.pure(stop)
    }
  }
}