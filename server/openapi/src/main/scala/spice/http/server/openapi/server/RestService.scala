package spice.http.server.openapi.server

import cats.effect.IO
import fabric.rw.RW
import spice.http.HttpMethod
import spice.http.content.Content

abstract class RestService[Request, Response](implicit val requestRW: RW[Request], val responseRW: RW[Response]) extends Service {
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