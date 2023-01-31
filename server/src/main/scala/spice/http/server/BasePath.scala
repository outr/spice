package spice.http.server

import spice.http.HttpExchange
import spice.net.{URLPath, URL}

object BasePath {
  private val key: String = "spice.base.path"

  def get(exchange: HttpExchange): Option[URLPath] = exchange.store.get[URLPath](key)

  def set(exchange: HttpExchange, basePath: URLPath): Unit = exchange.store(key) = basePath
}