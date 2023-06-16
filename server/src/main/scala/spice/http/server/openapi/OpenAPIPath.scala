package spice.http.server.openapi

import fabric.rw._

case class OpenAPIPath(parameters: List[OpenAPISchema] = Nil,
                       get: Option[OpenAPIPathEntry] = None,
                       post: Option[OpenAPIPathEntry] = None,
                       put: Option[OpenAPIPathEntry] = None)

object OpenAPIPath {
  implicit val rw: RW[OpenAPIPath] = RW.gen
}