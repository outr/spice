package spice.http.server.openapi

import fabric.define.DefType
import fabric.rw._
import fabric._

case class OpenAPIContentType(schema: OpenAPISchema,
                              example: Option[String] = None,
                              examples: Map[String, OpenAPIValue] = Map.empty)

object OpenAPIContentType {
  implicit val rw: RW[OpenAPIContentType] = RW.gen
}