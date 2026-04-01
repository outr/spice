package spice.openapi

import fabric.rw.*

case class OpenAPISecurityScheme(`type`: String,
                                 scheme: Option[String] = None,
                                 bearerFormat: Option[String] = None,
                                 description: Option[String] = None,
                                 name: Option[String] = None,
                                 in: Option[String] = None,
                                 openIdConnectUrl: Option[String] = None) derives RW
