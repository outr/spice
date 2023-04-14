package spice.http.server.config

import spice.http.HttpExchange
import spice.net.{URLPath, URL}

trait ServerSocketListener {
  def host: String
  def port: Option[Int]
  def enabled: Boolean
  def basePath: URLPath
  def urls: List[URL]
  def description: Option[String]

  def baseUrlFor(url: URL): Option[URL] = {
    val s = url.toString
    urls.find(u => s.startsWith(u.toString))
  }

  def matches(url: URL): Boolean = baseUrlFor(url).nonEmpty
}

object ServerSocketListener {
  private val key: String = "spice.listener"

  def get(exchange: HttpExchange): Option[ServerSocketListener] = exchange.store.get[ServerSocketListener](key)

  def set(exchange: HttpExchange, listener: ServerSocketListener): Unit = exchange.store(key) = listener
}