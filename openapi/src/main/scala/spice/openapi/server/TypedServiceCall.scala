package spice.openapi.server

import rapid._
import fabric.rw._
import scribe.mdc.MDC
import spice.http.HttpMethod
import spice.net.ContentType

case class TypedServiceCall[Req, Res](call: ServiceRequest[Req] => Task[ServiceResponse[Res]],
                                      method: HttpMethod,
                                      responseTypes: List[ResponseType] = List(ResponseType(ContentType.`application/json`)),
                                      summary: String,
                                      description: String,
                                      successDescription: String,
                                      service: Service,
                                      override val tags: List[String],
                                      override val operationId: Option[String],
                                      requestRW: RW[Req],
                                      responseRW: RW[Res],
                                      requestSchema: Option[Schema],
                                      responseSchema: Option[Schema]) extends ServiceCall {
  override type Request = Req
  override type Response = Res

  override def apply(request: ServiceRequest[Request])
                    (implicit mdc: MDC): Task[ServiceResponse[Response]] = call(request)
}
