package spice.http

import cats.effect.IO
import reactify.{Channel, Val, Var}

trait WebSocket {
  protected val _status: Var[ConnectionStatus] = Var(ConnectionStatus.Closed)
  val status: Val[ConnectionStatus] = _status

  val send: WebSocketChannels = new WebSocketChannels
  val receive: WebSocketChannels = new WebSocketChannels
  val error: Channel[Throwable] = Channel[Throwable]

  def connect(): IO[ConnectionStatus]

  def disconnect(): Unit
}