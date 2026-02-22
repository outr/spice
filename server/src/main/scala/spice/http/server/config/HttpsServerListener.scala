package spice.http.server.config

import fabric.rw.*
import spice.http.server.ServerUtil
import spice.net.*

case class HttpsServerListener(host: String = "127.0.0.1",
                               port: Option[Int] = Some(443),
                               keyStore: KeyStore = KeyStore(),
                               enabled: Boolean = false,
                               basePath: URLPath = path"",
                               description: Option[String] = None) extends ServerSocketListener {
  override lazy val urls: List[URL] = if (host == "0.0.0.0") {
    ServerUtil.localIPs().map { ip =>
      URL(protocol = Protocol.Https, host = ip.toString, path = basePath, port = port.getOrElse(0))
    }
  } else {
    List(URL(protocol = Protocol.Https, host = host, path = basePath, port = port.getOrElse(0)))
  }

  override def toString: String = urls.mkString(", ")
}

object HttpsServerListener {
  given rw: RW[HttpsServerListener] = RW.gen
}