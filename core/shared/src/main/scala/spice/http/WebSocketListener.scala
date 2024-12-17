package spice.http

import rapid.Task

class WebSocketListener extends WebSocket {
  override def connect(): Task[ConnectionStatus] = {
    _status @= ConnectionStatus.Open
    Task.pure(ConnectionStatus.Open)
  }

  override def disconnect(): Unit = {
    _status @= ConnectionStatus.Closed
    send.close @= ()
  }
}

object WebSocketListener {
  val key: String = "webSocketListener"
}