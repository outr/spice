package spice.openapi

import fabric.rw.*

case class OpenAPIResponse(description: String,
                           content: Option[OpenAPIContent] = None) derives RW
