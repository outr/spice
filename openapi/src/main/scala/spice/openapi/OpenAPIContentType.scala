package spice.openapi

import fabric.rw.*

/** A content-type entry under `responses[code].content` or `requestBody.content`.
  *
  * `schema` is used for normal request/response bodies (one body, one schema).
  *
  * `itemSchema` is the OpenAPI 3.2 keyword for streaming event sequences (e.g.
  * `text/event-stream`) — the schema describes the type of each individual
  * event, not the body as a whole. Mutually exclusive with `schema`. When
  * `itemSchema` is set, `schema` MUST be `None` (downstream codegens pick one
  * based on the media type semantics). */
case class OpenAPIContentType(schema: Option[OpenAPISchema] = None,
                              itemSchema: Option[OpenAPISchema] = None,
                              example: Option[String] = None,
                              examples: Map[String, OpenAPIValue] = Map.empty) derives RW
