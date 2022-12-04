package spice.http.server.openapi

import fabric._
import fabric.rw._
import spice.net.ContentType

case class OpenAPIContent(content: List[(ContentType, OpenAPIContentType)])

object OpenAPIContent {
  implicit val rw: RW[OpenAPIContent] = RW.from(
    r = c => obj(c.content.map {
      case (ct, oct) => ct.toString -> oct.json
    }: _*),
    w = j => OpenAPIContent(j.asMap.map {
      case (ct, oct) => ContentType.parse(ct) -> oct.as[OpenAPIContentType]
    }.toList)
  )

  def apply(content: (ContentType, OpenAPIContentType)*): OpenAPIContent = OpenAPIContent(content.toList)
}