package spice.openapi

import fabric.rw.*

case class OpenAPIContentType(schema: OpenAPISchema,
                              example: Option[String] = None,
                              examples: Map[String, OpenAPIValue] = Map.empty) derives RW
