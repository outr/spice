package spice.http.server.openapi

import fabric.rw.RW

case class OpenAPIParameter(description: String,
                            name: String,
                            in: String,
                            required: Boolean,
                            schema: OpenAPISchema)

object OpenAPIParameter {
  implicit val rw: RW[OpenAPIParameter] = RW.gen
}