package spice.http.server.dsl

import cats.effect.IO
import scribe.data.MDC
import spice.http.HttpExchange

import scala.util.matching.Regex

case class PathRegexFilter(regex: Regex) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[FilterResponse] = IO {
    val path = exchange.request.url.path.decoded
    if (path.matches(regex.regex)) {
      FilterResponse.Continue(exchange)
    } else {
      FilterResponse.Stop(exchange)
    }
  }
}