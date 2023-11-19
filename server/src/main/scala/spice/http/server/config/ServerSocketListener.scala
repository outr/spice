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
    val s = url.toString.toLowerCase
    urls.find(u => s.startsWith(u.toString.toLowerCase))
  }

  def matches(url: URL): Boolean = baseUrlFor(url).nonEmpty

  def unapply(url: URL): Option[URLPath] = baseUrlFor(url)
    .map(baseUrl => URLPath(url.path.parts.drop(baseUrl.path.parts.length)))
}

object ServerSocketListener {
  private val key: String = "spice.listener"

  def get(exchange: HttpExchange): Option[ServerSocketListener] = exchange.store.get[ServerSocketListener](key)

  def set(exchange: HttpExchange, listener: ServerSocketListener): Unit = exchange.store(key) = listener
}