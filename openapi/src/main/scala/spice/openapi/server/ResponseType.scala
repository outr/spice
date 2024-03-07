package spice.openapi.server

import spice.net.ContentType

case class ResponseType(contentType: ContentType, format: Option[String] = None)