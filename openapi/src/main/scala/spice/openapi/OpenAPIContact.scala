package spice.openapi

import fabric.rw.*

case class OpenAPIContact(name: Option[String] = None,
                          url: Option[String] = None,
                          email: Option[String] = None) derives RW
