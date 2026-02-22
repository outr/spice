package spice.http.server.dsl

import rapid.Task
import scribe.mdc.MDC
import spice.http.HttpExchange

case class ListConnectionFilter(filters: List[ConnectionFilter]) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)
                    (using mdc: MDC): Task[FilterResponse] = firstPath(exchange, filters)

  private def firstPath(exchange: HttpExchange,
                        filters: List[ConnectionFilter])
                       (using mdc: MDC): Task[FilterResponse] = if (filters.isEmpty) {
    Task.pure(FilterResponse.Stop(exchange))
  } else {
    val filter = filters.head
    filter.apply(exchange).flatMap {
      case r: FilterResponse.Continue => firstPath(r.exchange, filters.tail)
      case r: FilterResponse.Stop => firstPath(r.exchange, filters.tail)
    }
  }
}