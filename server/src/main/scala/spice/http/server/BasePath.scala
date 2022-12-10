package spice.http.server

import spice.http.HttpExchange
import spice.net.{Path, URL}

object BasePath {
  private val key: String = "spice.base.path"

  def get(exchange: HttpExchange): Option[Path] = exchange.store.get[Path](key)

  def set(exchange: HttpExchange, basePath: Path): Unit = exchange.store(key) = basePath
}