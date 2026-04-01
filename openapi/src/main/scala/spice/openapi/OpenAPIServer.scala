package spice.openapi

import fabric.rw.*
import spice.net.URL

case class OpenAPIServer(url: URL,
                         description: Option[String] = None,
                         variables: Map[String, OpenAPIServerVariable] = Map.empty) derives RW
