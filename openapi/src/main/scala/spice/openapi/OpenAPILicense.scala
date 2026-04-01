package spice.openapi

import fabric.rw.*

case class OpenAPILicense(name: String,
                          url: Option[String] = None) derives RW
