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

  /** Send a request and return a stream of lines paired with a handle to abort the in-flight call.
    * The default implementation delegates to `sendStream` with a no-op cancel — backends that can
    * abort the underlying call (OkHttp `Call.cancel()`, Netty channel close, java.net.http stream
    * close) override this to wire the cancellation through. */
  def sendStreamHandle(request: HttpRequest): Task[StreamHandle[String]] =
    sendStream(request).map(stream => StreamHandle(stream, Task.unit))

  def webSocket(url: URL): WebSocket

  def dispose(): Task[Unit]
}