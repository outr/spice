package spice.openapi

import fabric.*
import fabric.filter.{RemoveEmptyFilter, RemoveNullsFilter}
import fabric.io.{JsonFormatter, YamlFormatter}
import fabric.rw.*

case class OpenAPI(openapi: String = "3.2.0",
                   info: OpenAPIInfo,
                   tags: List[OpenAPITag] = Nil,
                   servers: List[OpenAPIServer] = Nil,
                   paths: Map[String, OpenAPIPath] = Map.empty,
                   components: Option[OpenAPIComponents] = None) {
  lazy val asJson: Json = OpenAPI.asJson(this)
  lazy val asJsonString: String = JsonFormatter.Default(asJson)
  lazy val asYaml: String = YamlFormatter(asJson)

  def componentByRef(ref: String): Option[OpenAPISchema] = {
    val name = ref.substring(ref.lastIndexOf('/') + 1)
    components.flatMap(_.schemas.get(name))
  }

  override def toString: String = asJsonString
}

object OpenAPI {
  given rw: RW[OpenAPI] = RW.gen

  private def asJson(api: OpenAPI): Json = api
    .json
    .filterOne(RemoveNullsFilter)
    .filterOne(RemoveEmptyFilter)
    .filterOne(OpenAPI32Filter)
}
