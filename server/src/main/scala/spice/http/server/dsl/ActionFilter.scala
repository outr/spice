package spice.http.server.dsl

import rapid.Task
import scribe.mdc.MDC
import spice.http.HttpExchange

class ActionFilter(f: HttpExchange => Task[HttpExchange]) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
    f(exchange).map(FilterResponse.Continue.apply)
  }
}

object ActionFilter {
  def apply(f: HttpExchange => Task[HttpExchange]): ActionFilter = new ActionFilter(f)
}