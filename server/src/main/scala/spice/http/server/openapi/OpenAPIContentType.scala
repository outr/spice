package spice.http.server.openapi

import fabric.rw.RW

case class OpenAPIContentType(schema: Either[OpenAPIComponentSchema, OpenAPISchema],
                              example: Option[String] = None,
                              examples: Map[String, OpenAPIValue] = Map.empty)

object OpenAPIContentType {
  implicit val rw: RW[OpenAPIContentType] = RW.gen
}