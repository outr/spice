package spice.http.server.openapi.server

import cats.effect.IO
import fabric.rw._
import spice.http.HttpExchange
import spice.http.content.Content

case class ServiceRequest[Request](request: Request, exchange: HttpExchange) {
  def response[Response](response: Response)
                        (implicit rw: RW[Response]): IO[ServiceResponse[Response]] = {
    exchange.withContent(Content.json(response.json)).map { exchange =>
      ServiceResponse[Response](exchange)
    }
  }
}