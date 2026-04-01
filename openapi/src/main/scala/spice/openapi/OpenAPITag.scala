package spice.openapi

import fabric.rw.*

case class OpenAPITag(name: String,
                      description: Option[String] = None,
                      externalDocs: Option[OpenAPISchema.ExternalDocs] = None) derives RW
