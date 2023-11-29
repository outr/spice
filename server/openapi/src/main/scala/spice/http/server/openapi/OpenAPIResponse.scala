package spice.http.server.openapi

import fabric.rw._

case class OpenAPIResponse(description: String, content: OpenAPIContent)

object OpenAPIResponse {
  implicit val rw: RW[OpenAPIResponse] = RW.gen
}