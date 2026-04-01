package spice.openapi

import fabric.rw.*

case class OpenAPIComponents(parameters: Map[String, OpenAPIParameter] = Map.empty,
                             schemas: Map[String, OpenAPISchema] = Map.empty,
                             securitySchemes: Map[String, OpenAPISecurityScheme] = Map.empty) derives RW
