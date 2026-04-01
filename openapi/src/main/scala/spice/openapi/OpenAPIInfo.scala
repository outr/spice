package spice.openapi

import fabric.rw.*

case class OpenAPIInfo(title: String,
                       version: String,
                       description: Option[String] = None,
                       termsOfService: Option[String] = None,
                       contact: Option[OpenAPIContact] = None,
                       license: Option[OpenAPILicense] = None) derives RW
