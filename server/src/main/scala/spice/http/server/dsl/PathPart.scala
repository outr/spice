package spice.http.server.dsl

import spice.http.HttpExchange

object PathPart {
  private val Key: String = "PathPart"

  def fulfilled(exchange: HttpExchange): Boolean = exchange.store.getOrElse[List[String]](Key, Nil).isEmpty

  def take(exchange: HttpExchange, part: String): Option[HttpExchange] = {
    val parts = exchange.store.getOrElse(Key, exchange.request.url.path.parts)
    if (parts.nonEmpty && parts.head.value == part) {
      exchange.store(Key) = parts.tail
      Some(exchange)
    } else {
      None
    }
  }
}
