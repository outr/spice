package spice.http.server.dsl

import cats.effect.IO
import spice.http.HttpExchange

class ConditionalFilter(f: HttpExchange => Boolean) extends ConnectionFilter {
  override def filter(exchange: HttpExchange): IO[FilterResponse] = IO {
    if (f(exchange)) {
      FilterResponse.Continue(exchange)
    } else {
      FilterResponse.Stop(exchange)
    }
  }
}
