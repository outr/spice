package spice.openapi

import fabric.rw.*

case class OpenAPIResponse(description: String, content: OpenAPIContent)

object OpenAPIResponse {
  given rw: RW[OpenAPIResponse] = RW.gen
}