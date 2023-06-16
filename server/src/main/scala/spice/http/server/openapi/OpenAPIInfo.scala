package spice.http.server.openapi

import fabric.rw._

case class OpenAPIInfo(title: String,
                       version: String,
                       description: Option[String] = None)

object OpenAPIInfo {
  implicit val rw: RW[OpenAPIInfo] = RW.gen
}