package spice.openapi

import fabric.rw._

case class OpenAPIValue(value: String)

object OpenAPIValue {
  implicit val rw: RW[OpenAPIValue] = RW.gen
}