package spice.http.client

import cats.effect.IO
import spice.http.{HttpRequest, HttpResponse}

import scala.util.Try

trait HttpClientInstance {
  def send(request: HttpRequest): IO[Try[HttpResponse]]

  def dispose(): IO[Unit]
}