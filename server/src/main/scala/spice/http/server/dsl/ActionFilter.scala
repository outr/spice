package spice.http.server.dsl

import cats.effect.IO
import scribe.data.MDC
import spice.http.HttpExchange

class ActionFilter(f: HttpExchange => IO[HttpExchange]) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[FilterResponse] = {
    f(exchange).map(FilterResponse.Continue.apply)
  }
}

object ActionFilter {
  def apply(f: HttpExchange => IO[HttpExchange]): ActionFilter = new ActionFilter(f)
}