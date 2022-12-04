package spice.http.server.openapi.server

import cats.effect.IO
import fabric._
import fabric.io.JsonParser
import fabric.rw._
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.handler.HttpHandler
import spice.http.server.openapi._
import spice.http.server.rest.RestfulHandler.jsonFromContent
import spice.net
import spice.net.{ContentType, interpolation}

import scala.collection.immutable.ListMap
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

  def exampleRequest: Request
  def exampleResponse: Response

  implicit def requestRW: RW[Request]
  implicit def responseRW: RW[Response]

  def apply(request: ServiceRequest[Request]): IO[ServiceResponse[Response]]

  override def handle(exchange: HttpExchange): IO[HttpExchange] = {
    val args = exchange.request.url.path.extractArguments(service.path).toList.map {
      case (key, value) => key -> Try(JsonParser(value)).getOrElse(Str(value))
    }
    val argsJson = obj(args: _*)
    // TODO: Support GET params
    val contentJson = exchange.request.content.map(jsonFromContent).flatMap(_.toOption).getOrElse(obj())
    val requestJson = if (argsJson.isEmpty) {
      contentJson
    } else {
      argsJson.merge(contentJson)
    }
    val request = requestJson.as[Request]
    apply(ServiceRequest[Request](request, exchange)).map(_.exchange)
  }

  lazy val openAPI: Option[OpenAPIPathEntry] = if (this eq ServiceCall.NotSupported) {
    None
  } else {
    Some(OpenAPIPathEntry(
      summary = summary,
      description = description,
      tags = tags,
      operationId = operationId,
      requestBody = None, // TODO: Implement
      responses = Map(
        "200" -> OpenAPIResponse(
          description = successDescription,
          content = OpenAPIContent(
            ContentType.`application/json` -> OpenAPIContentType(
              schema = Left(schemaFrom(exampleResponse.json))
            )
          )
        )
        // TODO: Support errors
      )
    ))
  }

  private def schemaFrom(json: Json): OpenAPIComponentSchema = json.`type` match {
    case t if t.is(JsonType.Arr) => OpenAPIComponentSchema(
      `type` = "array",
      items = Some(Left(schemaFrom(json.asVector.head)))
    )
    case t if t.is(JsonType.Str) => OpenAPIComponentSchema(
      `type` = "string"
    )
    case t => throw new UnsupportedOperationException(s"JSON type $t is not supported!")
  }
}

object ServiceCall {
  private val svc = new Service {
    override val path: net.Path = path"/"
  }

  val NotSupported: ServiceCall = svc.serviceCall[Unit, Unit](
    summary = "",
    description = "",
    successDescription = "",
    exampleRequest = (),
    exampleResponse = ()
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

case class TypedServiceCall[Req, Res](call: ServiceRequest[Req] => IO[ServiceResponse[Res]],
                                      summary: String,
                                      description: String,
                                      successDescription: String,
                                      service: Service,
                                      override val tags: List[String],
                                      override val operationId: Option[String],
                                      requestRW: RW[Req],
                                      responseRW: RW[Res],
                                      exampleRequest: Req,
                                      exampleResponse: Res) extends ServiceCall {
  override type Request = Req
  override type Response = Res

  override def apply(request: ServiceRequest[Request]): IO[ServiceResponse[Response]] = call(request)
}