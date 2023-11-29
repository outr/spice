package spice.openapi

import fabric._
import fabric.filter.{RemoveEmptyFilter, RemoveNullsFilter}
import fabric.io.{JsonFormatter, YamlFormatter}
import fabric.rw._

case class OpenAPI(openapi: String = "3.0.3",
                   info: OpenAPIInfo,
                   tags: List[OpenAPITag] = Nil,
                   servers: List[OpenAPIServer] = Nil,
                   paths: Map[String, OpenAPIPath] = Map.empty,
                   components: Option[OpenAPIComponents] = None) {
  lazy val asJson: Json = OpenAPI.asJson(this)
  lazy val asJsonString: String = JsonFormatter.Default(asJson)
  lazy val asYaml: String = YamlFormatter(asJson)

  override def toString: String = asJsonString
}

object OpenAPI {
  implicit val rw: RW[OpenAPI] = RW.gen

  private def asJson(api: OpenAPI): Json = api
    .json
    .filterOne(RemoveNullsFilter)
    .filterOne(RemoveEmptyFilter)
}