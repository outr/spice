package spice.server.handler

import spice.http.{HttpRequest, HttpResponse}

import scala.concurrent.Future

trait HttpHandler {
  def apply(request: HttpRequest, response: HttpResponse): Future[HttpResponse]
}

object HttpHandler {

}