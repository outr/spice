package spice.openapi

import fabric.define.DefType
import fabric.obj
import fabric.rw._
import spice.http.HttpMethod

case class OpenAPIPath(parameters: List[OpenAPISchema] = Nil,
                       methods: Map[HttpMethod, OpenAPIPathEntry] = Map.empty)

object OpenAPIPath {
  implicit val rw: RW[OpenAPIPath] = RW.from[OpenAPIPath](
    r = path => obj(
      "parameters" -> path.parameters.json :: path.methods.toList.map {
        case (method, entry) => method.value.toLowerCase -> entry.json
      }: _*
    ),
    w = json => OpenAPIPath(
      parameters = json("parameters").as[List[OpenAPISchema]],
      methods = json.asObj.value.filterNot(_._1 == "parameters").map {
        case (m, e) => HttpMethod(m) -> e.as[OpenAPIPathEntry]
      }
    ),
    d = DefType.Json
  )
}