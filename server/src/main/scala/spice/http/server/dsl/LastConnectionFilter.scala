package spice.http.server.dsl

import rapid.Task
import scribe.mdc.MDC
import spice.http.HttpExchange

case class LastConnectionFilter(filters: ConnectionFilter*) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(implicit mdc: MDC): Task[FilterResponse] = Task {
    last(exchange, filters*)
    FilterResponse.Continue(exchange)
  }
}