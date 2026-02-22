package spice.openapi.server

import rapid.*
import fabric.rw.*
import spice.http.HttpMethod
import spice.http.content.Content
import spice.net.{ContentType, URLPath}

abstract class RestService extends Service {
  type Request
  type Response

  protected def responseTypes: List[ResponseType] = List(ResponseType(ContentType.`application/json`))

  given requestRW: RW[Request]

  given responseRW: RW[Response]

  override val calls: List[ServiceCall] = List(
    serviceCall[Request, Response](
      method = HttpMethod.Post,
      responseTypes = responseTypes,
      summary = summary,
      description = description,
      successDescription = "OK"
    )(call)
  )

  protected def summary: String

  protected def description: String = summary

  protected def apply(request: Request): Task[Response]

  def call(request: ServiceRequest[Request]): Task[ServiceResponse[Response]] = apply(request.request).flatMap { response =>
    val content = response match {
      case content: Content => content
      case _ => Content.jsonFrom(response)
    }
    request.exchange.withContent(content).map(exchange => ServiceResponse[Response](exchange))
  }
}

object RestService {
  def apply[Req, Res](server: OpenAPIHttpServer,
                      urlPath: URLPath,
                      serviceSummary: String,
                      types: List[ResponseType] = List(ResponseType(ContentType.`application/json`)))
                     (f: Req => Task[Res])
                     (using reqRW: RW[Req], resRW: RW[Res]): TypedRestService[Req, Res] = TypedRestService[Req, Res](
    server = server,
    path = urlPath,
    summary = serviceSummary,
    responseTypes = types,
    f = f
  )
}