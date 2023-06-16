package spice.http.server.openapi

import fabric.rw._

case class OpenAPIComponents(parameters: Map[String, OpenAPIParameter],
                             schemas: Map[String, OpenAPIComponentSchema])

object OpenAPIComponents {
  implicit val rw: RW[OpenAPIComponents] = RW.gen
}