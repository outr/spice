package spice.http.server.openapi.server

import cats.effect.IO
import fabric.rw.RW

case class TypedServiceCall[Req, Res](call: ServiceRequest[Req] => IO[ServiceResponse[Res]],
                                      summary: String,
                                      description: String,
                                      successDescription: String,
                                      service: Service,
                                      override val tags: List[String],
                                      override val operationId: Option[String],
                                      requestRW: RW[Req],
                                      responseRW: RW[Res]) extends ServiceCall {
  override type Request = Req
  override type Response = Res

  override def apply(request: ServiceRequest[Request]): IO[ServiceResponse[Response]] = call(request)
}