package spice.http.server.openapi

import fabric.rw.RW

case class OpenAPIInfo(title: String,
                       version: String,
                       description: Option[String] = None)

object OpenAPIInfo {
  implicit val rw: RW[OpenAPIInfo] = RW.gen
}