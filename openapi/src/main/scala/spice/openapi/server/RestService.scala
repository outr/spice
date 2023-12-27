package spice.openapi.server

import cats.effect.IO
import fabric.rw.RW
import spice.http.HttpMethod
import spice.http.content.Content
import spice.net.URLPath

abstract class RestService extends Service {
  type Request
  type Response

  implicit def requestRW: RW[Request]
  implicit def responseRW: RW[Response]

  override val calls: List[ServiceCall] = List(
    serviceCall[Request, Response](
      method = HttpMethod.Post,
      summary = summary,
      description = description,
      successDescription = "OK"
    )(call)
  )

  protected def summary: String

  protected def description: String = summary

  protected def apply(request: Request): IO[Response]

  def call(request: ServiceRequest[Request]): IO[ServiceResponse[Response]] = apply(request.request).flatMap { response =>
    request.exchange.withContent(Content.jsonFrom(response)).map(exchange => ServiceResponse[Response](exchange))
  }
}

object RestService {
  def apply[Req, Res](urlPath: URLPath, serviceSummary: String)
                     (f: Req => IO[Res])
                     (implicit reqRW: RW[Req], resRW: RW[Res]): RestService = new RestService {
    override type Request = Req
    override type Response = Res

    override implicit def requestRW: RW[Request] = reqRW
    override implicit def responseRW: RW[Response] = resRW

    override val path: URLPath = urlPath
    override protected def summary: String = serviceSummary

    override protected def apply(request: Request): IO[Response] = f(request)
  }
}