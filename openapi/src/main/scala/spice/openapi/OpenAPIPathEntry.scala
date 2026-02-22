package spice.openapi

import fabric.rw.*

case class OpenAPIPathEntry(summary: String,
                            description: String,
                            tags: List[String] = Nil,
                            operationId: Option[String] = None,
                            requestBody: Option[OpenAPIRequestBody] = None,
                            responses: Map[String, OpenAPIResponse])

object OpenAPIPathEntry {
  given rw: RW[OpenAPIPathEntry] = RW.gen
}