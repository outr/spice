package spice.http.server.config

import fabric.rw.RW
import spice.http.server.ServerUtil

case class HttpsServerListener(host: String = "127.0.0.1",
                               port: Int = 8443,
                               keyStore: KeyStore = KeyStore(),
                               enabled: Boolean = false) extends ServerSocketListener {
  override def toString: String = if (host == "0.0.0.0") {
    s"HTTPS ${ServerUtil.localIPs().map(ip => s"$ip:$port").mkString(", ")}"
  } else {
    s"HTTPS $host:$port"
  }
}

object HttpsServerListener {
  implicit val rw: RW[HttpsServerListener] = RW.gen
}