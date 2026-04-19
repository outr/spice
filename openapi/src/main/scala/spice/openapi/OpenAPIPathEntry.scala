package spice.openapi

import fabric.rw.*

case class OpenAPIPathEntry(summary: String,
                            description: String,
                            tags: List[String] = Nil,
                            operationId: Option[String] = None,
                            requestBody: Option[OpenAPIRequestBody] = None,
                            responses: Map[String, OpenAPIResponse],
                            parameters: List[OpenAPIParameter] = Nil,
                            security: Option[List[Map[String, List[String]]]] = None,
                            deprecated: Option[Boolean] = None) derives RW
