package spice.openapi

import fabric.rw.*

case class OpenAPIInfo(title: String,
                       version: String,
                       description: Option[String] = None)

object OpenAPIInfo {
  given rw: RW[OpenAPIInfo] = RW.gen
}