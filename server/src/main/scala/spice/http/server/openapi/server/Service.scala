package spice.http.server.openapi.server

import cats.effect.IO
import fabric.rw.RW
import spice.http.{HttpExchange, HttpMethod}
import spice.net.Path

trait Service {
  val path: Path

  val get: ServiceCall = ServiceCall.NotSupported
  val post: ServiceCall = ServiceCall.NotSupported
  val put: ServiceCall = ServiceCall.NotSupported

  def apply(exchange: HttpExchange): Option[ServiceCall] = exchange.request.method match {
    case HttpMethod.Get if get != ServiceCall.NotSupported => Some(get)
    case HttpMethod.Post if post != ServiceCall.NotSupported => Some(post)
    case HttpMethod.Put if put != ServiceCall.NotSupported => Some(put)
    case _ => None    // TODO: Support all methods
  }

  def serviceCall[Request, Response](summary: String,
                                     description: String,
                                     successDescription: String,
                                     tags: List[String] = Nil,
                                     operationId: Option[String] = None,
                                     requestSchema: Option[Schema] = None,
                                     responseSchema: Option[Schema] = None)
                                    (call: ServiceRequest[Request] => IO[ServiceResponse[Response]])
                                    (implicit requestRW: RW[Request], responseRW: RW[Response]): ServiceCall = {
    TypedServiceCall[Request, Response](
      call = call,
      summary = summary,
      description = description,
      successDescription = successDescription,
      service = this,
      tags = tags,
      operationId = operationId,
      requestRW = requestRW,
      responseRW = responseRW,
      requestSchema = requestSchema,
      responseSchema = responseSchema
    )
  }
}