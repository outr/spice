package spice.http.durable

import fabric.*
import fabric.io.JsonParser
import fabric.rw.*
import rapid.Task
import reactify.{Channel, Val, Var}
import spice.http.{ConnectionStatus, WebSocket}

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.*

class DurableSocketClient[Id: RW, Event: RW, Info: RW](
  createWebSocket: () => WebSocket,
  config: DurableSocketConfig = DurableSocketConfig(),
  outboundLog: EventLog[Id, Event],
  initialChannelId: Id,
  info: Info,
  clientId: String = UUID.randomUUID().toString
) {
  // Per-channel last-seen seq tracking (for switch resume)
  private val channelSeqs = new ConcurrentHashMap[Id, Long]()

  val protocol: DurableSocket[Id, Event, Info] = new DurableSocket[Id, Event, Info](config, outboundLog, initialChannelId) {
    override protected def handleHandshakeMessage(json: Json, msgType: String): Unit = {
      msgType match {
        case "connected" =>
          val lastClientSeq = json("lastClientSeq").asLong
          val resumed = json("resumed").asBoolean
          handleConnectedResponse(lastClientSeq, resumed)
        case "switched" =>
          val newChannelId = json("channelId").as[Id]
          val lastSeq = json("lastSeq").asLong
          // Save the old channel's last-seen seq before switching
          channelSeqs.put(channelId, highestProcessedSeq)
          handleSwitchComplete(newChannelId, lastSeq)
        case _ =>
      }
    }

    override protected def handleDisconnect(): Unit = {
      super.handleDisconnect()
      if (protocol.state() != ProtocolState.Closed) {
        attemptReconnect()
      }
    }
  }

  val reconnectAttempt: Var[Int] = Var(0)

  def connect(): Task[Unit] = {
    val ws = createWebSocket()
    ws.connect().flatMap { status =>
      if (status == ConnectionStatus.Open) {
        protocol.bind(ws)
        protocol.sendConnect(clientId, info)
        waitForActive()
      } else {
        Task.error(new RuntimeException(s"WebSocket connection failed: $status"))
      }
    }
  }

  def close(): Unit = protocol.close()

  def invoke(tool: String, args: Json): Task[Json] = protocol.invoke(tool, args)
  val onEvent: Channel[(Long, Event)] = protocol.onEvent
  val state: Val[ProtocolState] = protocol.state

  def switch(newChannelId: Id): Task[Unit] = {
    val lastSeq = channelSeqs.getOrDefault(newChannelId, 0L)
    protocol.switch(newChannelId, lastSeq)
  }

  private def handleConnectedResponse(lastClientSeq: Long, resumed: Boolean): Unit = {
    if (resumed) {
      protocol.replayAfter(lastClientSeq).start()
    }
    reconnectAttempt @= 0
    protocol.activate()
  }

  private def attemptReconnect(): Unit = {
    val attempt = reconnectAttempt()
    config.reconnectStrategy.nextDelay(attempt) match {
      case None =>
        protocol.close()
      case Some(delay) =>
        reconnectAttempt @= attempt + 1
        Task.sleep(delay).flatMap { _ =>
          if (protocol.state() == ProtocolState.Closed) Task.unit
          else {
            val ws = createWebSocket()
            ws.connect().flatMap { status =>
              if (status == ConnectionStatus.Open) {
                protocol.bind(ws)
                protocol.sendResume(clientId, protocol.highestProcessedSeq, info)
                waitForActive()
              } else {
                attemptReconnect()
                Task.unit
              }
            }.handleError { _ =>
              attemptReconnect()
              Task.unit
            }
          }
        }.start()
    }
  }

  private def waitForActive(): Task[Unit] = {
    if (protocol.state() == ProtocolState.Active) Task.unit
    else Task.sleep(50.millis).flatMap(_ => waitForActive())
  }
}
