package spice.http.server.config

import spice.net.URL

trait ServerSocketListener {
  def host: String
  def port: Int
  def enabled: Boolean
  def urls: List[URL]
  def description: Option[String]
}