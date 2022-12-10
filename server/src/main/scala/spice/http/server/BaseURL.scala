package spice.http.server

import spice.http.HttpExchange
import spice.net.URL

object BaseURL {
  private val key: String = "spice.base.url"

  def get(exchange: HttpExchange): Option[URL] = exchange.store.get[URL](key)

  def set(exchange: HttpExchange, baseURL: URL): Unit = exchange.store(key) = baseURL
}