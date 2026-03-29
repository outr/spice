package spice.http.client

import rapid.{Stream, Task}
import spice.http.{HttpRequest, HttpResponse, WebSocket}
import spice.net.URL

import scala.util.Try

trait HttpClientInstance {
  def send(request: HttpRequest): Task[Try[HttpResponse]]

  /** Send a request and return the response body as a stream of lines.
    * Used for Server-Sent Events (SSE) and NDJSON streaming protocols.
    * The default implementation falls back to buffered send + line splitting.
    * JVM implementation uses InputStream for true line-by-line streaming. */
  def sendStream(request: HttpRequest): Task[Stream[String]] =
    send(request).flatMap {
      case scala.util.Success(response) =>
        response.content match {
          case Some(content) => content.asString.map { body =>
            Stream.emits(body.split('\n').toSeq)
          }
          case None => Task.pure(Stream.emits(Seq.empty[String]))
        }
      case scala.util.Failure(e) => Task.error(e)
    }

  def webSocket(url: URL): WebSocket

  def dispose(): Task[Unit]
}