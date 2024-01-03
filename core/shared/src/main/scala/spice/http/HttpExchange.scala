package spice.http

import cats.effect.IO
import spice.UserException
import spice.http.content.Content
import spice.net.URLPath
import spice.store.{MapStore, Store}

case class HttpExchange(request: HttpRequest,
                        response: HttpResponse,
                        path: URLPath,
                        store: Store,
                        finished: Boolean) {
  def modify(f: HttpResponse => IO[HttpResponse]): IO[HttpExchange] = {
    f(response).map(r => copy(response = r))
  }

  def withContent(content: Content): IO[HttpExchange] = modify { response =>
    IO.pure(response.withContent(content))
  }

  def isWebSocketUpgradeRequest: Boolean = Headers.`Connection`.all(request.headers).exists(_.equalsIgnoreCase("Upgrade"))

  def webSocketListener: Option[WebSocketListener] = store.get[WebSocketListener](WebSocketListener.key)

  def withWebSocketListener(): IO[(HttpExchange, WebSocketListener)] = {
    if (isWebSocketUpgradeRequest) {
      val listener = new WebSocketListener
      store.update(WebSocketListener.key, listener)
      modify { response =>
        IO(response.withStatus(HttpStatus.SwitchingProtocols))
      }.map(exchange => (exchange, listener))
    } else {
      throw UserException(s"Not a WebSocket upgrade request! Expected 'Connection' set to 'Upgrade'. Headers: ${request.headers}")
    }
  }

  def finish(): HttpExchange = copy(finished = true)
}

object HttpExchange {
  def apply(request: HttpRequest): HttpExchange = HttpExchange(
    request = request,
    response = HttpResponse(),
    path = request.url.path,
    store = new MapStore(),
    finished = false
  )
}