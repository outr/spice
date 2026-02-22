package spice.openapi.server

import rapid.*
import fabric.rw.*
import scribe.mdc.MDC
import spice.http.HttpMethod
import spice.net.ContentType
import spice.openapi.OpenAPIResponse

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
                                      responseSchema: Option[Schema],
                                      override val errorResponses: Map[String, OpenAPIResponse] = Map.empty) extends ServiceCall {
  override type Request = Req
  override type Response = Res

  override def apply(request: ServiceRequest[Request])
                    (using mdc: MDC): Task[ServiceResponse[Response]] = call(request)
}
