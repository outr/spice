package spice.http.server.config

import fabric.rw.RW
import spice.http.server.ServerUtil
import spice.net.{Path, URL, interpolation}

case class HttpServerListener(host: String = "127.0.0.1",
                              port: Int = 8080,
                              enabled: Boolean = true,
                              basePath: Path = path"/",
                              description: Option[String] = None) extends ServerSocketListener {
  override lazy val urls: List[URL] = if (host == "0.0.0.0") {
    ServerUtil.localIPs().map { ip =>
      URL(host = ip.toString, port = port, path = basePath)
    }
  } else {
    List(URL(host = host, port = port, path = basePath))
  }

  override def toString: String = urls.mkString(", ")
}

object HttpServerListener {
  implicit val rw: RW[HttpServerListener] = RW.gen
}