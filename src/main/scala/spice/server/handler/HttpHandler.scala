package spice.server.handler

import spice.http.{HttpRequest, HttpResponse}

trait HttpHandler {
  def apply(request: HttpRequest): HttpResponse
}