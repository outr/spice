package spice.openapi

import fabric.rw.*

case class OpenAPIParameter(description: String,
                            name: String,
                            in: String,
                            required: Boolean,
                            schema: OpenAPISchema)

object OpenAPIParameter {
  given rw: RW[OpenAPIParameter] = RW.gen
}