package spice.http.durable

import fabric.*
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw.*
import rapid.Task
import reactify.{Channel, Val, Var}
import spice.http.WebSocket

class DurableSocket[Id: RW, Event: RW, Info: RW](
  val config: DurableSocketConfig,
  val outboundLog: EventLog[Id, Event],
  initialChannelId: Id
) {
  private val _channelId: Var[Id] = Var(initialChannelId)
  def channelId: Id = _channelId()

  // --- Reactive channels ---
  val onEvent: Channel[(Long, Event)] = Channel[(Long, Event)]
  val onError: Channel[ErrorMessage] = Channel[ErrorMessage]
  val onEphemeral: Channel[Json] = Channel[Json]

  private val _state: Var[ProtocolState] = Var(ProtocolState.Disconnected)
  val state: Val[ProtocolState] = _state

  // --- Internal state ---
  private val rawWs: Var[Option[WebSocket]] = Var(None)
  private val remoteAckedSeq: Var[Long] = Var(0L)
  private val tracker = new SequenceTracker(config)

  @volatile private var pendingSwitch: rapid.task.Completable[Unit] = scala.compiletime.uninitialized

  // --- Sending: Events (durable, EventLog seq) ---

  def push(event: Event): Task[Long] = {
    outboundLog.append(channelId, event).map { seq =>
      sendLogged(seq, event)
      seq
    }
  }

  def sendLogged(seq: Long, event: Event): Unit = {
    val eventJson = JsonFormatter.Default(obj(
      "type" -> str("event"),
      "seq" -> num(seq),
      "data" -> event.json
    ))
    sendRaw(eventJson)
  }

  // --- Sending: Ephemeral (not logged) ---

  def sendEphemeral(json: Json): Unit = {
    sendRaw(JsonFormatter.Default(json))
  }

  // --- Channel switching ---

  def switch(newChannelId: Id, lastSeqForChannel: Long = 0L): Task[Unit] = Task.defer {
    val completable = Task.completable[Unit]
    pendingSwitch = completable
    val msg = JsonFormatter.Default(obj(
      "type" -> str("switch"),
      "channelId" -> newChannelId.json,
      "lastSeq" -> num(lastSeqForChannel)
    ))
    sendRaw(msg)
    completable
  }

  def handleSwitchComplete(newChannelId: Id, lastSeq: Long): Unit = {
    _channelId @= newChannelId
    tracker.reset(lastSeq)
    val c = pendingSwitch
    if (c != null) {
      pendingSwitch = null
      c.success(())
    }
  }

  def updateChannelId(newChannelId: Id): Unit = {
    _channelId @= newChannelId
    tracker.reset(0L)
  }

  // --- Lifecycle ---

  def bind(ws: WebSocket): Unit = {
    rawWs @= Some(ws)
    ws.receive.text.attach(handleRawMessage)
    ws.receive.close.attach(_ => handleDisconnect())
    ws.status.attach {
      case spice.http.ConnectionStatus.Closed => handleDisconnect()
      case _ =>
    }
  }

  def unbind(): Unit = {
    stopTimers()
    rawWs @= None
    if (_state() == ProtocolState.Active) {
      _state @= ProtocolState.Disconnected
    }
  }

  def close(): Unit = {
    _state @= ProtocolState.Closed
    val ws = rawWs()
    unbind()
    ws.foreach(_.disconnect())
  }

  // --- Protocol internals ---

  def sendConnect(clientId: String, info: Info): Unit = {
    _state @= ProtocolState.Handshaking
    sendRaw(JsonFormatter.Default(obj(
      "type" -> str("connect"),
      "clientId" -> str(clientId),
      "info" -> info.json
    )))
  }

  def sendResume(clientId: String, lastSeq: Long, info: Info): Unit = {
    _state @= ProtocolState.Handshaking
    sendRaw(JsonFormatter.Default(obj(
      "type" -> str("resume"),
      "clientId" -> str(clientId),
      "lastSeq" -> num(lastSeq),
      "info" -> info.json
    )))
  }

  def sendConnected(lastClientSeq: Long, resumed: Boolean): Unit = {
    sendRaw(JsonFormatter.Default(obj(
      "type" -> str("connected"),
      "lastClientSeq" -> num(lastClientSeq),
      "resumed" -> bool(resumed)
    )))
  }

  def activate(): Unit = {
    _state @= ProtocolState.Active
    startTimers()
  }

  def replayAfter(seq: Long): Task[Unit] = {
    outboundLog.replay(channelId, seq).map { events =>
      events.foreach { case (eventSeq, event) =>
        sendLogged(eventSeq, event)
      }
    }
  }

  def highestProcessedSeq: Long = tracker.highestProcessedSeq

  def sendAck(): Unit = {
    val seq = tracker.highestProcessedSeq
    if (seq > 0) {
      sendRaw(JsonFormatter.Default(obj("type" -> str("ack"), "seq" -> num(seq))))
      tracker.resetAckCount()
    }
  }

  // --- Internal: message dispatch ---

  private def handleRawMessage(text: String): Unit = {
    val json = JsonParser(text)
    json.get("type").map(_.asString) match {
      case Some("event") =>
        val seq = json("seq").asLong
        if (tracker.acceptInbound(seq)) {
          val event = json("data").as[Event]
          onEvent @= (seq, event)
          maybeAck()
        }

      case Some("ack") =>
        remoteAckedSeq @= json("seq").asLong

      case Some("connect" | "resume" | "connected" | "switch" | "switched") =>
        handleHandshakeMessage(json, json("type").asString)

      case Some("error") =>
        onError @= ErrorMessage(json("code").asString, json("message").asString)

      case _ =>
        onEphemeral @= json
    }
  }

  protected def handleHandshakeMessage(json: Json, msgType: String): Unit = {}

  protected def handleDisconnect(): Unit = {
    if (_state() != ProtocolState.Closed) {
      _state @= ProtocolState.Disconnected
    }
    stopTimers()
  }

  private def maybeAck(): Unit = {
    if (tracker.shouldSendAck) sendAck()
  }

  // --- Timers ---

  private var timerGeneration: Long = 0L

  private def startTimers(): Unit = {
    timerGeneration += 1
    ackTimerLoop(timerGeneration).start()
  }

  private def stopTimers(): Unit = {
    timerGeneration += 1
  }

  private def ackTimerLoop(gen: Long): Task[Unit] =
    Task.sleep(config.ackBatchDelay).flatMap { _ =>
      if (_state() == ProtocolState.Active && gen == timerGeneration) {
        sendAck()
        ackTimerLoop(gen)
      } else Task.unit
    }

  // --- Raw send ---

  protected[durable] def sendRaw(text: String): Unit = {
    rawWs().foreach(ws => ws.send.text @= text)
  }
}
