package spice.openapi.server

import cats.effect.IO
import fabric.rw.RW
import spice.http.HttpMethod
import spice.http.content.Content
import spice.net.{ContentType, URLPath}

abstract class RestService extends Service {
  type Request
  type Response

  protected def responseTypes: List[ResponseType] = List(ResponseType(ContentType.`application/json`))

  implicit def requestRW: RW[Request]
  implicit def responseRW: RW[Response]

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

  protected def apply(request: Request): IO[Response]

  def call(request: ServiceRequest[Request]): IO[ServiceResponse[Response]] = apply(request.request).flatMap { response =>
    val content = response match {
      case content: Content => content
      case _ => Content.jsonFrom(response)
    }
    request.exchange.withContent(content).map(exchange => ServiceResponse[Response](exchange))
  }
}

object RestService {
  def apply[Req, Res](urlPath: URLPath,
                      serviceSummary: String,
                      types: List[ResponseType] = List(ResponseType(ContentType.`application/json`)))
                     (f: Req => IO[Res])
                     (implicit reqRW: RW[Req], resRW: RW[Res]): RestService = new RestService {
    override type Request = Req
    override type Response = Res
    override protected def responseTypes: List[ResponseType] = types

    override implicit def requestRW: RW[Request] = reqRW
    override implicit def responseRW: RW[Response] = resRW

    override val path: URLPath = urlPath
    override protected def summary: String = serviceSummary

    override protected def apply(request: Request): IO[Response] = f(request)
  }
}