package spice.http.server.openapi

import fabric.rw._

case class OpenAPIRequestBody(required: Boolean, content: OpenAPIContent)

object OpenAPIRequestBody {
  implicit val rw: RW[OpenAPIRequestBody] = RW.gen
}