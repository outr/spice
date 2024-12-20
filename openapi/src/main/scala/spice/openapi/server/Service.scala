package spice.openapi.server

import rapid._
import fabric.rw._
import scribe.mdc.MDC
import spice.http.content.Content
import spice.http.server.handler.HttpHandler
import spice.http.{HttpExchange, HttpMethod}
import spice.net.{ContentType, URLPath}

trait Service extends HttpHandler {
  val path: URLPath
  val calls: List[ServiceCall]

  override def handle(exchange: HttpExchange)(implicit mdc: MDC): Task[HttpExchange] = apply(exchange) match {
    case Some(sc) => sc.handle(exchange)
    case None => Task.pure(exchange)
  }

  def apply(exchange: HttpExchange): Option[ServiceCall] = {
    if (exchange.path == path) {
      calls.find(sc => sc.method == exchange.request.method || exchange.request.method == HttpMethod.Get)
    } else {
      None
    }
  }

  def serviceCall[Request, Response](method: HttpMethod,
                                     responseTypes: List[ResponseType] = List(ResponseType(ContentType.`application/json`)),
                                     summary: String,
                                     description: String,
                                     successDescription: String,
                                     tags: List[String] = Nil,
                                     operationId: Option[String] = None,
                                     requestSchema: Option[Schema] = None,
                                     responseSchema: Option[Schema] = None)
                                    (call: ServiceRequest[Request] => Task[ServiceResponse[Response]])
                                    (implicit requestRW: RW[Request], responseRW: RW[Response]): ServiceCall = {
    TypedServiceCall[Request, Response](
      call = call,
      method = method,
      responseTypes = responseTypes,
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