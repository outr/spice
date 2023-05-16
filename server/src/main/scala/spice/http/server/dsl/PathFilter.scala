package spice.http.server.dsl

import cats.effect.IO
import scribe.data.MDC
import spice.http.HttpExchange
import spice.net.URLPath

case class PathFilter(path: URLPath) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[FilterResponse] = IO {
    if (path == exchange.request.url.path) {
      val args = path.extractArguments(exchange.request.url.path)
      if (args.nonEmpty) {
        exchange.store(PathFilter.Key) = args
      }
      FilterResponse.Continue(exchange)
    } else {
      FilterResponse.Stop(exchange)
    }
  }
}

object PathFilter {
  val Key: String = "pathArguments"

  def argumentsFromConnection(exchange: HttpExchange): Map[String, String] = {
    exchange.store.getOrElse[Map[String, String]](Key, Map.empty)
  }
}