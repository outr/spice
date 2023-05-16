package spice.http.server.dsl

import cats.effect.IO
import scribe.data.MDC
import spice.http.HttpExchange
import spice.http.server.handler.HttpHandler

trait ConnectionFilter extends HttpHandler {
  def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[FilterResponse]

  protected def continue(exchange: HttpExchange): FilterResponse = FilterResponse.Continue(exchange)
  protected def stop(exchange: HttpExchange): FilterResponse = FilterResponse.Stop(exchange)

  def /(that: ConnectionFilter): ConnectionFilter = CombinedConnectionFilter(this, that)

  def /(filters: Seq[ConnectionFilter]): ConnectionFilter = this / ListConnectionFilter(filters.toList)

  /**
    * Adds a filter to apply at the very end, presuming this wasn't canceled.
    *
    * @param exchange the exchange to attach to
    * @param filters the filters to run last
    */
  def last(exchange: HttpExchange, filters: ConnectionFilter*): Unit = {
    val current = exchange.store.getOrElse[List[ConnectionFilter]](ConnectionFilter.LastKey, Nil)
    exchange.store(ConnectionFilter.LastKey) = current ::: filters.toList
  }

  override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = {
    apply(exchange).flatMap {
      case FilterResponse.Continue(c) => {
        val last = c.store.getOrElse[List[ConnectionFilter]](ConnectionFilter.LastKey, Nil)
        ConnectionFilter.recurse(c, last).map(_.exchange)
      }
      case FilterResponse.Stop(c) => IO.pure(c)
    }
  }
}

object ConnectionFilter {
  private val LastKey: String = "ConnectionFilterLast"

  def recurse(exchange: HttpExchange, filters: List[ConnectionFilter]): IO[FilterResponse] = if (filters.isEmpty) {
    IO.pure(FilterResponse.Continue(exchange))
  } else {
    val filter = filters.head
    filter.apply(exchange).flatMap {
      case stop: FilterResponse.Stop => IO.pure(stop)
      case FilterResponse.Continue(c) => recurse(c, filters.tail)
    }
  }
}