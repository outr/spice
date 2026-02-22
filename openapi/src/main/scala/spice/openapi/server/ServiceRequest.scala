package spice.openapi.server

import rapid.*
import fabric.rw.*
import spice.http.HttpExchange
import spice.http.content.Content

case class ServiceRequest[Request](request: Request, exchange: HttpExchange) {
  def response[Response](response: Response)
                        (using rw: RW[Response]): Task[ServiceResponse[Response]] = {
    exchange.withContent(Content.json(response.json)).map { exchange =>
      ServiceResponse[Response](exchange)
    }
  }
}