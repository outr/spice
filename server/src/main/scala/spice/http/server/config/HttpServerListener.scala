package spice.http.server.config

import fabric.rw.RW
import spice.http.server.ServerUtil

case class HttpServerListener(host: String = "127.0.0.1",
                              port: Int = 8080,
                              enabled: Boolean = true) extends ServerSocketListener {
  override def toString: String = if (host == "0.0.0.0") {
    s"HTTP ${ServerUtil.localIPs().map(ip => s"$ip:$port").mkString(", ")}"
  } else {
    s"HTTP $host:$port"
  }
}

object HttpServerListener {
  implicit val rw: RW[HttpServerListener] = RW.gen
}