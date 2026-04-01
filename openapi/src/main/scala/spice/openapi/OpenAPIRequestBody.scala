package spice.openapi

import fabric.rw.*

case class OpenAPIRequestBody(required: Boolean,
                              content: OpenAPIContent,
                              description: Option[String] = None) derives RW
