package spice.openapi.server

import cats.effect.IO
import fabric._
import fabric.define.DefType
import fabric.io.JsonParser
import fabric.rw._
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpMethod, HttpStatus}
import spice.http.content.Content
import spice.http.server.BasePath
import spice.http.server.handler.HttpHandler
import spice.http.server.openapi._
import spice.http.server.rest.Restful
import spice.net
import spice.net._
import spice.openapi.{OpenAPIContent, OpenAPIContentType, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema}

import scala.util.Try

trait ServiceCall extends HttpHandler {
  type Request
  type Response
  def method: HttpMethod

  def summary: String
  def description: String
  def tags: List[String] = Nil
  def operationId: Option[String] = None
  def successDescription: String

  def service: Service

  implicit def requestRW: RW[Request]
  implicit def responseRW: RW[Response]

  def requestSchema: Option[Schema]
  def responseSchema: Option[Schema]

  def apply(request: ServiceRequest[Request])(implicit mdc: MDC): IO[ServiceResponse[Response]]

  override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = {
    // Merge the base path of the listener (if defined) to the service path
    val actualPath = BasePath.get(exchange) match {
      case Some(basePath) => basePath.merge(service.path)
      case None => service.path
    }
    val args = exchange.request.url.path.extractArguments(actualPath).toList.map {
      case (key, value) => key -> Try(JsonParser(value)).getOrElse(Str(value))
    }
    val argsJson = obj(args: _*)
    Restful.jsonFromExchange(exchange).flatMap { contentJson =>
      val requestJson = if (argsJson.isEmpty) {
        contentJson
      } else {
        argsJson.merge(contentJson)
      }
      val request = requestJson.as[Request]
      apply(ServiceRequest[Request](request, exchange)).map(_.exchange)
    }
  }

  lazy val openAPI: Option[OpenAPIPathEntry] = {
    val requestBody = if (requestRW.definition == DefType.Null || method == HttpMethod.Get) {
      None
    } else {
      Some(OpenAPIRequestBody(
        required = true,
        content = OpenAPIContent(
          ContentType.`application/json` -> OpenAPIContentType(
            schema = schemaFrom(requestRW.definition, requestSchema.getOrElse(Schema()), nullable = false)
          )
        )
      ))
    }
    Some(OpenAPIPathEntry(
      summary = summary,
      description = description,
      tags = tags,
      operationId = operationId,
      requestBody = requestBody,
      responses = Map(
        "200" -> OpenAPIResponse(
          description = successDescription,
          content = OpenAPIContent(
            ContentType.`application/json` -> OpenAPIContentType(
              schema = schemaFrom(responseRW.definition, responseSchema.getOrElse(Schema()), nullable = false)
            )
          )
        )
        // TODO: Support errors
      )
    ))
  }

  private def componentSchema(schema: Schema, map: Map[String, DefType], nullable: Boolean): OpenAPISchema = {
    val c = if (map.keySet == Set("[key]")) {
      val t = map("[key]")
      OpenAPISchema.Component(
        `type` = "object",
        additionalProperties = Some(schemaFrom(t, Schema(), nullable))
      )
    } else {
      OpenAPISchema.Component(
        `type` = "object",
        properties = map.map {
          case (key, t) => key -> schemaFrom(t, schema.properties.getOrElse(key, Schema()), nullable)
        }
      )
    }
    if (nullable) {
      c.makeNullable
    } else {
      c
    }
  }

  private def schemaFrom(dt: DefType, schema: Schema, nullable: Boolean): OpenAPISchema = (dt match {
    case DefType.Obj(map, None) => componentSchema(schema, map, nullable)
    case DefType.Obj(map, Some(className)) =>
      OpenAPIHttpServer.register(className)(componentSchema(schema, map, nullable))
      val index = className.lastIndexOf('.')
      val name = if (index != -1) {
        className.substring(index + 1)
      } else {
        className
      }
      OpenAPISchema.Ref(s"#/components/schemas/$name")
    case DefType.Arr(t) => OpenAPISchema.Component(
      `type` = "array",
      items = Some(schemaFrom(t, schema.items.getOrElse(Schema()), nullable))
    )
    case DefType.Str => OpenAPISchema.Component(
      `type` = "string"
    )
    case DefType.Enum(values) => OpenAPISchema.Component(
      `type` = "string",
      `enum` = values
    )
    case DefType.Bool => OpenAPISchema.Component(
      `type` = "boolean"
    )
    case DefType.Int => OpenAPISchema.Component(
      `type` = "integer"
    )
    case DefType.Dec => OpenAPISchema.Component(
      `type` = "number"
    )
    case DefType.Opt(t) => schemaFrom(t, schema, nullable = true)
    case DefType.Null => OpenAPISchema.Component(
      `type` = "null"
    )
    case DefType.Poly(values) => OpenAPISchema.OneOf(values.values.map(dt => schemaFrom(dt, schema, nullable)).toList)
    case DefType.Json => OpenAPISchema.Component(
      `type` = "json"
    )
    case _ => throw new UnsupportedOperationException(s"DefType not supported: $dt")
  }).withSchema(schema)
}