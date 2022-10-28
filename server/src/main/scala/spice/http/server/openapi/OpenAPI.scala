package spice.http.server.openapi

import fabric.io.YamlFormatter
import fabric.rw._
import spice.net._

case class OpenAPI(openapi: String = "3.0.0",
                   info: OpenAPIInfo,
                   servers: List[OpenAPIServer],
                   paths: Map[String, Map[String, OpenAPIPath]])

object OpenAPI {
  implicit val rw: RW[OpenAPI] = RW.gen
}

case class OpenAPIInfo(title: String,
                       description: String,
                       version: String)

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
        description = "Optional multiline or single-line description in [CommonMark](http://commonmark.org/help/) or HTML.",
        version = "0.1.9"
      ),
      servers = List(
        OpenAPIServer(
          url = url"http://api.example.com/v1",
          description = "Optional server description, e.g. Main (production) server"
        ),
        OpenAPIServer(
          url = url"http://staging-api.example.com",
          description = "Optional server description, e.g. Internal staging server for testing"
        )
      ),
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