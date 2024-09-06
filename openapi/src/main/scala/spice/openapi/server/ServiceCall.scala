package spice.openapi.server

import cats.effect.IO
import fabric._
import fabric.define.DefType
import fabric.io.JsonParser
import fabric.rw._
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpMethod}
import spice.http.server.BasePath
import spice.http.server.handler.HttpHandler
import spice.http.server.rest.Restful
import spice.net._
import spice.openapi.{OpenAPIContent, OpenAPIContentType, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema}

import scala.util.Try

trait ServiceCall extends HttpHandler {
  type Request
  type Response
  def method: HttpMethod
  def responseTypes: List[ResponseType]

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

  private def hasFile(map: Map[String, DefType]): Boolean = {
    map.values.exists(dt => dt.className.contains("spice.http.server.rest.FileUpload"))
  }

  lazy val openAPI: Option[OpenAPIPathEntry] = {
    val contentType = requestRW.definition match {
      case DefType.Obj(map, _) if hasFile(map) => ContentType.`multipart/form-data`
      case _ => ContentType.`application/json`
    }
    val requestBody = if (requestRW.definition == DefType.Null || method == HttpMethod.Get) {
      None
    } else {
      Some(OpenAPIRequestBody(
        required = true,
        content = OpenAPIContent(
          contentType -> OpenAPIContentType(
            schema = if (contentType == ContentType.`multipart/form-data`) {
              val map = requestRW.definition.asInstanceOf[DefType.Obj].map
              componentSchema(requestSchema.getOrElse(Schema()), map, None, nullable = None)
            } else {
              schemaFrom(requestRW.definition, requestSchema.getOrElse(Schema()), None, nullable = None)
            }
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
            responseTypes.map { rt =>
              rt.contentType -> OpenAPIContentType(
                schema = schemaFrom(responseRW.definition, responseSchema.getOrElse(Schema()), rt.format, nullable = None)
              )
            }: _*
          )
        )
        // TODO: Support errors
      )
    ))
  }

  private def componentSchema(schema: Schema, map: Map[String, DefType], format: Option[String], nullable: Option[Boolean]): OpenAPISchema = {
    val c = if (map.keySet == Set("[key]")) {
      val t = map("[key]")
      OpenAPISchema.Component(
        `type` = "object",
        format = format,
        additionalProperties = Some(schemaFrom(t, Schema(), format, nullable))
      )
    } else {
      OpenAPISchema.Component(
        `type` = "object",
        format = format,
        properties = map.map {
          case (key, dt) => key -> schemaFrom(dt, schema.properties.getOrElse(key, Schema()), format, nullable)
        }
      )
    }
    if (nullable.getOrElse(false)) {
      c.makeNullable
    } else {
      c
    }
  }

  private def schemaFrom(dt: DefType, schema: Schema, format: Option[String], nullable: Option[Boolean]): OpenAPISchema = (dt match {
    case DefType.Obj(map, None) => componentSchema(schema, map, format, nullable)
    case DefType.Obj(_, Some("spice.http.server.rest.FileUpload")) => OpenAPISchema.Component(
      `type` = "string",
      format = Some("binary")
    )
    case DefType.Obj(map, Some(className)) =>
      val refName = OpenAPIHttpServer.register(className)(componentSchema(schema, map, format, None))
      OpenAPISchema.Ref(s"#/components/schemas/$refName", nullable)
    case DefType.Arr(t) => OpenAPISchema.Component(
      `type` = "array",
      format = format,
      items = Some(schemaFrom(t, schema.items.getOrElse(Schema()), format, None)),
      nullable = nullable
    )
    case DefType.Str => OpenAPISchema.Component(
      `type` = "string",
      format = format,
      nullable = nullable
    )
    case DefType.Enum(values, cn) => OpenAPISchema.Component(
      `type` = "string",
      description = cn,
      `enum` = values,
      format = format,
      nullable = nullable
    )
    case DefType.Bool => OpenAPISchema.Component(
      `type` = "boolean",
      format = format,
      nullable = nullable
    )
    case DefType.Int => OpenAPISchema.Component(
      `type` = "integer",
      format = format,
      nullable = nullable
    )
    case DefType.Dec => OpenAPISchema.Component(
      `type` = "number",
      format = format,
      nullable = nullable
    )
    case DefType.Opt(t) => schemaFrom(t, schema, format = format, nullable = Some(true))
    case DefType.Null => OpenAPISchema.Component(
      `type` = "null",
      format = format
    )
    case DefType.Poly(values, _) => OpenAPISchema.OneOf(
      schemas = values.values.map(dt => schemaFrom(dt, schema, format, nullable)).toList,
      nullable = nullable
    )
    case DefType.Json => OpenAPISchema.Component(
      `type` = "json",
      format = format,
      nullable = nullable
    )
    case _ => throw new UnsupportedOperationException(s"DefType not supported: $dt")
  }).withSchema(schema)
}