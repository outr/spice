package spice.openapi

import fabric.rw.*

case class OpenAPIServerVariable(default: String,
                                 description: Option[String] = None,
                                 `enum`: List[String] = Nil) derives RW
