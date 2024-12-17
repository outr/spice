package spice.http.server.dsl

import rapid.Task
import scribe.mdc.MDC
import spice.http.HttpExchange

class ConditionalFilter(f: HttpExchange => Boolean) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(implicit mdc: MDC): Task[FilterResponse] = Task {
    if (f(exchange)) {
      FilterResponse.Continue(exchange)
    } else {
      FilterResponse.Stop(exchange)
    }
  }
}
