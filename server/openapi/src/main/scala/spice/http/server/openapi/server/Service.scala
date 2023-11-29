package spice.http.server.openapi.server

import cats.effect.IO
import fabric.rw._
import spice.http.content.Content
import spice.http.{HttpExchange, HttpMethod}
import spice.net.URLPath

trait Service {
  val path: URLPath
  val calls: List[ServiceCall]

  def apply(exchange: HttpExchange): Option[ServiceCall] = {
    if (exchange.path == path) {
      calls.find(_.method == exchange.request.method)
    } else {
      None
    }
  }

  def serviceCall[Request, Response](method: HttpMethod,
                                     summary: String,
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
      method = method,
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