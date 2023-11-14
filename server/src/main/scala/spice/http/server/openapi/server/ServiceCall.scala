package spice.http.server.openapi.server

import cats.effect.IO
import fabric._
import fabric.define.DefType
import fabric.io.JsonParser
import fabric.rw._
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.BasePath
import spice.http.server.handler.HttpHandler
import spice.http.server.openapi._
import spice.http.server.rest.Restful
import spice.net
import spice.net._

import scala.util.Try

trait ServiceCall extends HttpHandler {
  type Request
  type Response

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

  lazy val openAPI: Option[OpenAPIPathEntry] = if (this eq ServiceCall.NotSupported) {
    None
  } else {
    Some(OpenAPIPathEntry(
      summary = summary,
      description = description,
      tags = tags,
      operationId = operationId,
      requestBody = Some(OpenAPIRequestBody(
        required = true,
        content = OpenAPIContent(
          ContentType.`application/json` -> OpenAPIContentType(
            schema = Left(schemaFrom(requestRW.definition, requestSchema.getOrElse(Schema())))
          )
        )
      )),
      responses = Map(
        "200" -> OpenAPIResponse(
          description = successDescription,
          content = OpenAPIContent(
            ContentType.`application/json` -> OpenAPIContentType(
              // TODO: Replace Left(OpenAPIComponentSchema) with Right(OpenAPISchema)
              schema = Left(schemaFrom(responseRW.definition, responseSchema.getOrElse(Schema())))
            )
          )
        )
        // TODO: Support errors
      )
    ))
  }

  private def schemaFrom(dt: DefType, schema: Schema): OpenAPIComponentSchema = (dt match {
    case DefType.Obj(map) => OpenAPIComponentSchema(
      `type` = "object",
      properties = map.map {
        case (key, t) => key -> Left(schemaFrom(t, schema.properties.getOrElse(key, Schema())))
      }
    )
    case DefType.Arr(t) => OpenAPIComponentSchema(
      `type` = "array",
      items = Some(Left(schemaFrom(t, schema.items.getOrElse(Schema()))))
    )
    case DefType.Str => OpenAPIComponentSchema(
      `type` = "string"
    )
    case DefType.Enum(values) => OpenAPIComponentSchema(
      `type` = "string",
      `enum` = values
    )
    case DefType.Bool => OpenAPIComponentSchema(
      `type` = "boolean"
    )
    case DefType.Int => OpenAPIComponentSchema(
      `type` = "integer"
    )
    case DefType.Dec => OpenAPIComponentSchema(
      `type` = "number"
    )
    case DefType.Opt(t) => schemaFrom(t, schema).copy(nullable = Some(true))
    case DefType.Null => OpenAPIComponentSchema(
      `type` = "null"
    )
    case _ => throw new UnsupportedOperationException(s"DefType not supported: $dt")
  }).copy(
    description = schema.description,
    maxLength = schema.maxLength,
    minimum = schema.minimum,
    maximum = schema.maximum,
    example = schema.example,
    maxItems = schema.maxItems,
    minItems = schema.minItems
  )
}

object ServiceCall {
  private val svc = new Service {
    override val path: net.URLPath = path"/"
  }

  val NotSupported: ServiceCall = svc.serviceCall[Unit, Unit](
    summary = "",
    description = "",
    successDescription = ""
  ) { request =>
    request.exchange.modify { response =>
      IO(response.withContent(Content.json(obj(
        "error" -> "Unsupported method"
      ))).withStatus(HttpStatus.MethodNotAllowed))
    }.map { exchange =>
      ServiceResponse[Unit](exchange)
    }
  }
}