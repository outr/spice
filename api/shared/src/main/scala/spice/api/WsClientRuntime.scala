package spice.api

import fabric.Json
import fabric.io.JsonParser
import rapid.Task
import spice.http.WebSocket
import spice.http.client.HttpClient
import spice.net.URL

object WsClientRuntime {
  def connect(wsUrl: URL, dispatch: (String, Json) => Unit): Task[WebSocket] = {
    val ws = HttpClient.url(wsUrl).webSocket()
    ws.receive.text.attach { text =>
      val json = JsonParser(text)
      val method = json("method").asString
      val args = json("args")
      dispatch(method, args)
    }
    ws.connect().map(_ => ws)
  }
}
