package spice.http.server.config

trait ServerSocketListener {
  def host: String
  def port: Int
  def enabled: Boolean
}