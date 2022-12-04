package spice.http.server.openapi

import fabric.rw.RW

case class OpenAPIValue(value: String)

object OpenAPIValue {
  implicit val rw: RW[OpenAPIValue] = RW.gen
}