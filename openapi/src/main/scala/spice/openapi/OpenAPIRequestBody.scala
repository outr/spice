package spice.openapi

import fabric.rw.*

case class OpenAPIRequestBody(required: Boolean, content: OpenAPIContent)

object OpenAPIRequestBody {
  given rw: RW[OpenAPIRequestBody] = RW.gen
}