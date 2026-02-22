package spice.openapi

import fabric.rw.*

case class OpenAPITag(name: String)

object OpenAPITag {
  given rw: RW[OpenAPITag] = RW.gen
}