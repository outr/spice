package spice.openapi

import fabric.rw.*

/**
 * Reusable definitions. `responses` is here for READING: spice never emits shared responses, but a
 * third-party spec keeps them in `components/responses` and refers to them by `$ref`, and without a
 * home for them the references could not be resolved.
 */
case class OpenAPIComponents(parameters: Map[String, OpenAPIParameter] = Map.empty,
                             schemas: Map[String, OpenAPISchema] = Map.empty,
                             responses: Map[String, OpenAPIResponse] = Map.empty,
                             securitySchemes: Map[String, OpenAPISecurityScheme] = Map.empty) derives RW
