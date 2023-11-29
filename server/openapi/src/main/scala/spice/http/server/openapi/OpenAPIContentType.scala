package spice.http.server.openapi

import fabric.rw._

case class OpenAPIContentType(schema: OpenAPISchema,
                              example: Option[String] = None,
                              examples: Map[String, OpenAPIValue] = Map.empty)

object OpenAPIContentType {
  implicit val rw: RW[OpenAPIContentType] = RW.gen
}