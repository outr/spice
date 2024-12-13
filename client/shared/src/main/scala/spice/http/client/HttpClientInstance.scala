package spice.http.client

import rapid.Task
import spice.http.{HttpRequest, HttpResponse, WebSocket}
import spice.net.URL

import scala.util.Try

trait HttpClientInstance {
  def send(request: HttpRequest): Task[Try[HttpResponse]]

  def webSocket(url: URL): WebSocket

  def dispose(): Task[Unit]
}