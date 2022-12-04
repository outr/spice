package spec

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import fabric.io.JsonFormatter
import fabric.{Json, JsonType, obj}
import fabric.rw._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.HttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.openapi._
import spice.net._

class OpenAPIServerSpec extends AsyncWordSpec with AsyncIOSpec with Matchers {
  "OpenAPIServer" should {
    "validate a proper swagger.yml file" in {
      val expected = TestUtils.loadYaml("openapi-simple.yml")
      val json = SimpleOpenAPIServer.api.asJson
      json should be(expected)
    }
  }

  object SimpleOpenAPIServer extends OpenAPIServer {
    config
      .clearListeners()
      .addListeners(
        HttpServerListener(
          host = "api.example.com",
          port = 80,
          basePath = path"/v1",
          description = Some("Optional server description, e.g. Main (production) server")
        ),
        HttpServerListener(
          host = "staging-api.example.com",
          port = 80,
          description = Some("Optional server description, e.g. Internal staging server for testing")
        )
      )

    override val title: String = "Sample API"
    override val version: String = "0.1.9"
    override val description: Option[String] = Some("Optional multiline or single-line description in [CommonMark](http://commonmark.org/help/) or HTML.")

    object usersService extends Service {
      override val path: Path = path"/users"
      override val get: ServiceCall = ServiceCall[Unit, List[String]](
        summary = "Returns a list of users.",
        description = "Optional extended description in CommonMark or HTML.",
        successDescription = "A JSON array of user names",
        exampleRequest = (),
        exampleResponse = List("username1", "username2", "username3")
      ) { request =>
        request.response(List("root", "john.doe"))
      }
    }

    override val services: List[Service] = List(
      usersService
    )
  }
}

trait OpenAPIServer extends HttpServer {
  def openAPIVersion: String = "3.1.0"

  def title: String
  def version: String
  def description: Option[String] = None
  def tags: List[String] = Nil

  def api: OpenAPI = OpenAPI(
    openapi = openAPIVersion,
    info = OpenAPIInfo(
      title = title,
      version = version,
      description = description
    ),
    tags = tags.map(OpenAPITag.apply),
    servers = config.listeners()flatMap { server =>
      server.urls.map { url =>
        OpenAPIServer(url = url, description = server.description)
      }
    },
    paths = services.map { service =>
      service.path.toString -> OpenAPIPath(
        parameters = Nil,  // TODO: Implement
        get = service.get.openAPI,
        post = service.post.openAPI,
        put = service.put.openAPI
      )
    }.toMap,
    components = None // TODO: Implement
  )

  def services: List[Service]

  // TODO: Implement
  override def handle(exchange: HttpExchange): IO[HttpExchange] = ???
}

trait Service {
  val path: Path

  val get: ServiceCall = ServiceCall.NotSupported
  val post: ServiceCall = ServiceCall.NotSupported
  val put: ServiceCall = ServiceCall.NotSupported
}

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
      requestBody = None,   // TODO: Implement
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

  lazy val NotSupported = apply[Unit, Unit]("", "", "", (), ()) { request =>
    request.exchange.modify { response =>
      IO(response.withContent(Content.json(obj(
        "error" -> "Unsupported method"
      ))).withStatus(HttpStatus.MethodNotAllowed))
    }.map { exchange =>
      ServiceResponse[Unit](exchange)
    }
  }
}

case class ServiceRequest[Request](request: Request, exchange: HttpExchange) {
  def response[Response](response: Response)
                        (implicit reader: Reader[Response]): IO[ServiceResponse[Response]] = {
    exchange.withContent(Content.json(response.json)).map { exchange =>
      ServiceResponse[Response](exchange)
    }
  }
}

case class ServiceResponse[Response](exchange: HttpExchange)