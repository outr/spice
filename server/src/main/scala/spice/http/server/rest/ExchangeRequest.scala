package spice.http.server.rest

import fabric.rw._
import spice.http.{HttpExchange, HttpRequest}

/**
 * Drop-in convenience class to wrap around an existing Request object and give a reference to the `HttpExchange`.
 */
case class ExchangeRequest[Request](request: Request,
                                    exchange: HttpExchange)

object ExchangeRequest {
  implicit def rw[Request: RW]: RW[ExchangeRequest[Request]] = RW.from[ExchangeRequest[Request]](
    r = req => req.request.json,
    w = json => ExchangeRequest[Request](
      request = json.as[Request],
      exchange = HttpExchange(HttpRequest())
    ),
    d = implicitly[RW[Request]].definition
  )
}