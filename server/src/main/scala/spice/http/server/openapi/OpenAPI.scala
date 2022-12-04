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

case class OpenAPIInfo(title: String,
                       version: String,
                       description: Option[String] = None)

object OpenAPIInfo {
  implicit val rw: RW[OpenAPIInfo] = RW.gen
}

case class OpenAPITag(name: String)

object OpenAPITag {
  implicit val rw: RW[OpenAPITag] = RW.gen
}

case class OpenAPIServer(url: URL, description: Option[String])

object OpenAPIServer {
  implicit val rw: RW[OpenAPIServer] = RW.gen
}

case class OpenAPIPath(parameters: List[OpenAPISchema] = Nil,
                       get: Option[OpenAPIPathEntry] = None,
                       post: Option[OpenAPIPathEntry] = None,
                       put: Option[OpenAPIPathEntry] = None)

object OpenAPIPath {
  implicit val rw: RW[OpenAPIPath] = RW.gen
}

case class OpenAPIPathEntry(summary: String,
                            description: String,
                            tags: List[String] = Nil,
                            operationId: Option[String] = None,
                            requestBody: Option[OpenAPIRequestBody] = None,
                            responses: Map[String, OpenAPIResponse])

object OpenAPIPathEntry {
  implicit val rw: RW[OpenAPIPathEntry] = RW.gen
}

case class OpenAPIRequestBody(required: Boolean, content: OpenAPIContent)

object OpenAPIRequestBody {
  implicit val rw: RW[OpenAPIRequestBody] = RW.gen
}

case class OpenAPIResponse(description: String, content: OpenAPIContent)

object OpenAPIResponse {
  implicit val rw: RW[OpenAPIResponse] = RW.gen
}

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

case class OpenAPIContentType(schema: Either[OpenAPIComponentSchema, OpenAPISchema],
                              example: Option[String] = None,
                              examples: Map[String, OpenAPIValue] = Map.empty)

object OpenAPIContentType {
  implicit val rw: RW[OpenAPIContentType] = RW.gen
}

case class OpenAPIValue(value: String)

object OpenAPIValue {
  implicit val rw: RW[OpenAPIValue] = RW.gen
}

case class OpenAPISchema(ref: String)

object OpenAPISchema {
  implicit val rw: RW[OpenAPISchema] = RW.from(
    r = s => obj("$ref" -> s.ref),
    w = j => OpenAPISchema(j("$ref").asString)
  )
}

case class OpenAPIComponents(parameters: Map[String, OpenAPIParameter],
                             schemas: Map[String, OpenAPIComponentSchema])

object OpenAPIComponents {
  implicit val rw: RW[OpenAPIComponents] = RW.gen
}

case class OpenAPIParameter(description: String,
                            name: String,
                            in: String,
                            required: Boolean,
                            schema: OpenAPISchema)

object OpenAPIParameter {
  implicit val rw: RW[OpenAPIParameter] = RW.gen
}

case class OpenAPIComponentSchema(`type`: String,
                                  description: Option[String] = None,
                                  maxLength: Option[Int] = None,
                                  minimum: Option[Int] = None,
                                  maximum: Option[Int] = None,
                                  example: Option[Json] = None,
                                  `enum`: List[Json] = Nil,
                                  maxItems: Option[Int] = None,
                                  minItems: Option[Int] = None,
                                  items: Option[Either[OpenAPIComponentSchema, OpenAPISchema]] = None,
                                  properties: Map[String, OpenAPISchema] = Map.empty)

object OpenAPIComponentSchema {
  implicit val itemRW: RW[Either[OpenAPIComponentSchema, OpenAPISchema]] = RW.from(
    r = {
      case Left(schema) => schema.json
      case Right(schema) => schema.json
    },
    w = json => json.get("type") match {
      case Some(_) => Left(json.as[OpenAPIComponentSchema])
      case None => Right(json.as[OpenAPISchema])
    }
  )
  implicit val rw: RW[OpenAPIComponentSchema] = RW.gen
}

//object Test2 {
//  def main(args: Array[String]): Unit = {
//    val api = OpenAPI(
//      info = OpenAPIInfo(
//        title = "Sample API",
//        version = "0.1.9",
//        description = Some("Optional multiline or single-line description in [CommonMark](http://commonmark.org/help/) or HTML.")
//      ),
//      servers = List(
//        OpenAPIServer(
//          url = url"https://api.example.com/v1",
//          description = "Optional server description, e.g. Main (production) server"
//        ),
//        OpenAPIServer(
//          url = url"https://staging-api.example.com",
//          description = "Optional server description, e.g. Internal staging server for testing"
//        )
//      ),
//      paths = Map(
//        "/users" -> Map(
//          "get" -> OpenAPIPath(
//            summary = "Returns a list of users.",
//            description = "Optional extended description in CommonMark or HTML.",
//            responses = Map(
//              "200" -> OpenAPIResponse(
//                description = "A JSON array of user names"
//              )
//            )
//          )
//        )
//      )
//    )
//
//    val json = api.json
//    val yamlString = YamlFormatter(json)
//    scribe.info(yamlString)
//  }
//}