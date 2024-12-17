package spice.http

import rapid.Task
import spice.UserException
import spice.http.content.Content
import spice.net.URLPath
import spice.store.{MapStore, Store}

case class HttpExchange(request: HttpRequest,
                        response: HttpResponse,
                        path: URLPath,
                        store: Store,
                        finished: Boolean) {
  def modify(f: HttpResponse => Task[HttpResponse]): Task[HttpExchange] = {
    f(response).map(r => copy(response = r))
  }

  def withContent(content: Content): Task[HttpExchange] = modify { response =>
    Task.pure(response.withContent(content))
  }

  def isWebSocketUpgradeRequest: Boolean = Headers.`Connection`.all(request.headers).exists(_.equalsIgnoreCase("Upgrade"))

  def webSocketListener: Option[WebSocketListener] = store.get[WebSocketListener](WebSocketListener.key)

  def withWebSocketListener(): Task[(HttpExchange, WebSocketListener)] = {
    if (isWebSocketUpgradeRequest) {
      val listener = new WebSocketListener
      store.update(WebSocketListener.key, listener)
      modify { response =>
        Task(response.withStatus(HttpStatus.SwitchingProtocols))
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