package spice.openapi

import fabric.rw.*

case class OpenAPIComponents(parameters: Map[String, OpenAPIParameter] = Map.empty,
                             schemas: Map[String, OpenAPISchema] = Map.empty)

object OpenAPIComponents {
  given rw: RW[OpenAPIComponents] = RW.gen
}