package spice.openapi

import fabric.rw.*
import spice.net.URL

case class OpenAPIServer(url: URL, description: Option[String])

object OpenAPIServer {
  given rw: RW[OpenAPIServer] = RW.gen
}