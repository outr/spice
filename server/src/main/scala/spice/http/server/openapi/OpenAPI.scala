package spice.http.server.openapi

import fabric._
import fabric.filter.{RemoveEmptyFilter, RemoveNullsFilter}
import fabric.io.{JsonFormatter, YamlFormatter}
import fabric.rw._
import spice.http.HttpStatus
import spice.net._

case class OpenAPI(openapi: String = "3.1.0",
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
    .filter(RemoveNullsFilter)
    .flatMap(_.filter(RemoveEmptyFilter))
    .get
}