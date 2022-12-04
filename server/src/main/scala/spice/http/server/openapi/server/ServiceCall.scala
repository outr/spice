package spice.http.server.openapi.server

import cats.effect.IO
import fabric._
import fabric.rw._
import spice.http.HttpStatus
import spice.http.content.Content
import spice.http.server.openapi._
import spice.net.ContentType

trait ServiceCall {
  type Request
  type Response

  def summary: String
  def description: String
  def tags: List[String] = Nil
  def operationId: Option[String] = None
  def successDescription: String

  def exampleRequest: Request
  def exampleResponse: Response

  implicit def requestRW: RW[Request]
  implicit def responseRW: RW[Response]

  def apply(request: ServiceRequest[Request]): IO[ServiceResponse[Response]]

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
  case class TypedServiceCall[Req, Res](call: ServiceRequest[Req] => IO[ServiceResponse[Res]],
                                        summary: String,
                                        description: String,
                                        successDescription: String,
                                        override val tags: List[String] = Nil,
                                        override val operationId: Option[String] = None,
                                        requestRW: RW[Req],
                                        responseRW: RW[Res],
                                        exampleRequest: Req,
                                        exampleResponse: Res) extends ServiceCall {
    override type Request = Req
    override type Response = Res

    override def apply(request: ServiceRequest[Request]): IO[ServiceResponse[Response]] = call(request)
  }

  def apply[Request, Response](summary: String,
                               description: String,
                               successDescription: String,
                               exampleRequest: Request,
                               exampleResponse: Response,
                               tags: List[String] = Nil,
                               operationId: Option[String] = None)
                              (call: ServiceRequest[Request] => IO[ServiceResponse[Response]])
                              (implicit requestRW: RW[Request], responseRW: RW[Response]): ServiceCall = {
    TypedServiceCall[Request, Response](
      call = call,
      summary = summary,
      description = description,
      successDescription = successDescription,
      tags = tags,
      operationId = operationId,
      requestRW = requestRW,
      responseRW = responseRW,
      exampleRequest = exampleRequest,
      exampleResponse = exampleResponse
    )
  }

  lazy val NotSupported: ServiceCall = apply[Unit, Unit](
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