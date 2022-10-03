package spice.http.server.dsl

import cats.effect.IO
import spice.http.HttpExchange

case class CombinedConnectionFilter(first: ConnectionFilter, second: ConnectionFilter) extends ConnectionFilter {
  override def filter(exchange: HttpExchange): IO[FilterResponse] = {
    first.filter(exchange).flatMap {
      case FilterResponse.Continue(c) => second.filter(c)
      case FilterResponse.Stop(c) => IO.pure(FilterResponse.Stop(c))
    }
  }
}