package spice.http.server.dsl

import spice.http.HttpExchange
import spice.net.{URLPath, URLPathPart}

object PathPart {
  private val Key: String = "PathPart"

  def fulfilled(exchange: HttpExchange): Boolean = {
    val path = exchange.store.getOrElse[URLPath](Key, URLPath.empty)
    path.parts match {
      case Nil | List(URLPathPart.Separator) => true
      case _ => false
    }
  }

  def take(exchange: HttpExchange, part: URLPath): Option[HttpExchange] = {
    val path = exchange.store.getOrElse(Key, exchange.request.url.path)
    path.take(part) match {
      case Some(remaining) =>
        exchange.store(Key) = remaining
        Some(exchange)
      case None => None
    }

//    scribe.info(s"Head: ${parts.headOption.map(_.value)}, Part: $part")
//    if (parts.nonEmpty && parts.head.value == part) {
//      exchange.store(Key) = parts.tail
//      Some(exchange)
//    } else {
//      None
//    }
  }
}
