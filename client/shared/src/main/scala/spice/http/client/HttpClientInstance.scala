package spice.http.client

import cats.effect.IO
import spice.http.{HttpRequest, HttpResponse, WebSocket}
import spice.net.URL

import scala.util.Try

trait HttpClientInstance {
  def send(request: HttpRequest): IO[Try[HttpResponse]]

  def webSocket(url: URL): WebSocket

  def dispose(): IO[Unit]
}