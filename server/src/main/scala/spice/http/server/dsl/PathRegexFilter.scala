package spice.http.server.dsl

import cats.effect.IO
import spice.http.HttpExchange

import scala.util.matching.Regex

case class PathRegexFilter(regex: Regex) extends ConnectionFilter {
  override def filter(exchange: HttpExchange): IO[FilterResponse] = IO {
    val path = exchange.request.url.path.decoded
    if (path.matches(regex.regex)) {
      FilterResponse.Continue(exchange)
    } else {
      FilterResponse.Stop(exchange)
    }
  }
}