package spice.api

import rapid.Task
import spice.net.URL

object WsClient {
  inline def connect[T](wsUrl: URL, handler: T): Task[spice.http.WebSocket] =
    ${ WsClientMacro.connect[T]('wsUrl, 'handler) }
}
