package spice.http.server
import cats.effect.IO
import spice.http.{Headers, HttpExchange}

trait CORSSupport extends HttpServer {
  protected def allowOrigin: String = "*"

  override protected def preHandle(exchange: HttpExchange): IO[HttpExchange] = super
    .preHandle(exchange)
    .flatMap { exchange =>
      exchange.modify { response =>
        IO.pure(response.withHeader(Headers.Response.`Access-Control-Allow-Origin`(allowOrigin)))
      }
    }
}
