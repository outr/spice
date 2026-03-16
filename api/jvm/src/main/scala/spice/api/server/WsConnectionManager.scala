package spice.api.server

import rapid.Task
import scribe.mdc.MDC
import spice.http.{ConnectionStatus, HttpExchange, WebSocketListener}
import spice.http.server.MutableHttpServer
import spice.http.server.handler.WebSocketHandler
import spice.net.{URL, URLMatcher, URLPath}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

class WsConnectionManager(server: MutableHttpServer, basePath: URLPath) {
  private val listeners = new ConcurrentLinkedQueue[WebSocketListener]()

  // Register the WebSocket handler on the server
  server.handler
    .matcher((url: URL) => url.path == basePath)
    .wrap(new WebSocketHandler {
      override def connect(exchange: HttpExchange, listener: WebSocketListener): Task[Unit] = Task {
        listeners.add(listener)
        scribe.info(s"WS client connected at $basePath (total: ${listeners.size()})")
        listener.receive.close.attach { _ =>
          listeners.remove(listener)
          scribe.info(s"WS client disconnected from $basePath (total: ${listeners.size()})")
        }
      }
    })

  def broadcast(message: String): Unit = {
    listeners.forEach { listener =>
      listener.send.text @= message
    }
  }

  def connectedCount: Int = listeners.size()
}
