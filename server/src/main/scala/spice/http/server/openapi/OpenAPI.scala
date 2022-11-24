package spice.http.server.openapi

import fabric.Json
import fabric.filter.RemoveNullsFilter
import fabric.io.{JsonFormatter, YamlFormatter}
import fabric.rw._
import spice.net._

case class OpenAPI(openapi: String = "3.1.0",
                   info: OpenAPIInfo,
                   servers: Option[List[OpenAPIServer]] = None,
                   paths: Map[String, Map[String, OpenAPIPath]] = Map.empty) {
  lazy val asJson: Json = OpenAPI.asJson(this)
  lazy val asJsonString: String = JsonFormatter.Default(asJson)

  override def toString: String = asJsonString
}

object OpenAPI {
  implicit val rw: RW[OpenAPI] = RW.gen

  private def asJson(api: OpenAPI): Json = api.json.filter(RemoveNullsFilter).get
}

case class OpenAPIInfo(title: String,
                       version: String,
                       description: Option[String] = None)

object OpenAPIInfo {
  implicit val rw: RW[OpenAPIInfo] = RW.gen
}

case class OpenAPIServer(url: URL, description: String)

object OpenAPIServer {
  implicit val rw: RW[OpenAPIServer] = RW.gen
}

case class OpenAPIPath(summary: String,
                       description: String,
                       responses: Map[String, OpenAPIResponse])

object OpenAPIPath {
  implicit val rw: RW[OpenAPIPath] = RW.gen
}

case class OpenAPIResponse(description: String)

object OpenAPIResponse {
  implicit val rw: RW[OpenAPIResponse] = RW.gen
}

object Test2 {
  def main(args: Array[String]): Unit = {
    val api = OpenAPI(
      info = OpenAPIInfo(
        title = "Sample API",
        version = "0.1.9",
        description = Some("Optional multiline or single-line description in [CommonMark](http://commonmark.org/help/) or HTML.")
      ),
      servers = Some(List(
        OpenAPIServer(
          url = url"https://api.example.com/v1",
          description = "Optional server description, e.g. Main (production) server"
        ),
        OpenAPIServer(
          url = url"https://staging-api.example.com",
          description = "Optional server description, e.g. Internal staging server for testing"
        )
      )),
      paths = Map(
        "/users" -> Map(
          "get" -> OpenAPIPath(
            summary = "Returns a list of users.",
            description = "Optional extended description in CommonMark or HTML.",
            responses = Map(
              "200" -> OpenAPIResponse(
                description = "A JSON array of user names"
              )
            )
          )
        )
      )
    )

    val json = api.json
    val yamlString = YamlFormatter(json)
    scribe.info(yamlString)
  }
}