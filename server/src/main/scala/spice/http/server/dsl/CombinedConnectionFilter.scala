package spice.http.server.dsl

import rapid.Task
import scribe.mdc.MDC
import spice.http.HttpExchange

case class CombinedConnectionFilter(first: ConnectionFilter, second: ConnectionFilter) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
    first(exchange).flatMap {
      case FilterResponse.Continue(c) => second.apply(c)
      case stop: FilterResponse.Stop => Task.pure(stop)
    }
  }
}