package spice.openapi

import fabric.rw.*

case class OpenAPIValue(value: String)

object OpenAPIValue {
  given rw: RW[OpenAPIValue] = RW.gen
}