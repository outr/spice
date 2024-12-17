package spice.http.server

import rapid.Task
import spice.http.{Headers, HttpExchange}

trait CORSSupport extends HttpServer {
  protected def allowOrigin: String = "*"

  override protected def preHandle(exchange: HttpExchange): Task[HttpExchange] = super
    .preHandle(exchange)
    .flatMap { exchange =>
      exchange.modify { response =>
        Task.pure(response.withHeader(Headers.Response.`Access-Control-Allow-Origin`(allowOrigin)))
      }
    }
}
