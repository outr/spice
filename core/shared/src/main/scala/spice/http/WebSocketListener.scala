package spice.http

import cats.effect.IO

class WebSocketListener extends WebSocket {
  override def connect(): IO[ConnectionStatus] = {
    _status @= ConnectionStatus.Open
    IO.pure(ConnectionStatus.Open)
  }

  override def disconnect(): Unit = {
    _status @= ConnectionStatus.Closed
    send.close @= ()
  }
}

object WebSocketListener {
  val key: String = "webSocketListener"
}