package spice.openapi

import fabric.rw._

case class OpenAPIComponents(parameters: Map[String, OpenAPIParameter] = Map.empty,
                             schemas: Map[String, OpenAPISchema] = Map.empty)

object OpenAPIComponents {
  implicit val rw: RW[OpenAPIComponents] = RW.gen
}