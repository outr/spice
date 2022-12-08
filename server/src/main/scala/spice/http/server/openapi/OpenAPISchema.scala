package spice.http.server.openapi

import fabric._
import fabric.define.DefType
import fabric.rw._

case class OpenAPISchema(ref: String)

object OpenAPISchema {
  implicit val rw: RW[OpenAPISchema] = RW.from(
    r = s => obj("$ref" -> s.ref),
    w = j => OpenAPISchema(j("$ref").asString),
    d = DefType.Obj("$ref" -> DefType.Str)
  )
}