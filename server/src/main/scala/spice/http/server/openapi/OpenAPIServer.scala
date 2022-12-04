package spice.http.server.openapi

import fabric.rw.RW
import spice.net.URL

case class OpenAPIServer(url: URL, description: Option[String])

object OpenAPIServer {
  implicit val rw: RW[OpenAPIServer] = RW.gen
}