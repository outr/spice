package spice.http.durable

import fabric.*
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw.*
import rapid.Task
import reactify.{Channel, Val, Var}
import spice.http.WebSocket

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters.*

class DurableSocket[Id: RW, Event: RW, Info: RW](
  val config: DurableSocketConfig,
  val outboundLog: EventLog[Id, Event],
  initialChannelId: Id
) {
  private val _channelId: Var[Id] = Var(initialChannelId)
  def channelId: Id = _channelId()
  // --- Reactive channels ---
  val onInvoke: Channel[InvokeMessage] = Channel[InvokeMessage]
  val onEvent: Channel[(Long, Event)] = Channel[(Long, Event)]
  val onError: Channel[ErrorMessage] = Channel[ErrorMessage]

  private val _state: Var[ProtocolState] = Var(ProtocolState.Disconnected)
  val state: Val[ProtocolState] = _state

  // --- Internal reactive state ---
  private val rawWs: Var[Option[WebSocket]] = Var(None)
  private val lastPongReceived: Var[Long] = Var(System.currentTimeMillis())
  private val remoteAckedSeq: Var[Long] = Var(0L)

  // Inbound tracking
  private val tracker = new SequenceTracker(config)

  // RPC correlation
  private val pendingInvocations = new ConcurrentHashMap[String, rapid.task.Completable[Json]]()
  private val invocationIdCounter = new AtomicLong(0L)

  // RPC outbound buffer (invoke/respond use local seq, not EventLog)
  private val rpcSeqCounter = new AtomicLong(0L)
  private val rpcOutBuffer = new ConcurrentLinkedQueue[(Long, String)]()

  // Pending channel switch
  @volatile private var pendingSwitch: rapid.task.Completable[Unit] = scala.compiletime.uninitialized

  // --- Sending: RPC (ephemeral, local seq) ---

  def invoke(tool: String, args: Json): Task[Json] = Task.defer {
    val id = s"req-${invocationIdCounter.incrementAndGet()}"
    val seq = rpcSeqCounter.incrementAndGet()
    val completable = Task.completable[Json]
    pendingInvocations.put(id, completable)
    val msgJson = JsonFormatter.Default(obj(
      "type" -> str("invoke"),
      "seq" -> num(seq),
      "id" -> str(id),
      "tool" -> str(tool),
      "args" -> args
    ))
    rpcOutBuffer.add((seq, msgJson))
    sendRaw(msgJson)
    completable
  }

  def respond(id: String, data: Json): Task[Long] = Task {
    val seq = rpcSeqCounter.incrementAndGet()
    val msgJson = JsonFormatter.Default(obj(
      "type" -> str("result"),
      "seq" -> num(seq),
      "id" -> str(id),
      "data" -> data
    ))
    rpcOutBuffer.add((seq, msgJson))
    sendRaw(msgJson)
    seq
  }

  def respondError(id: String, error: String): Task[Long] = Task {
    val seq = rpcSeqCounter.incrementAndGet()
    val msgJson = JsonFormatter.Default(obj(
      "type" -> str("result"),
      "seq" -> num(seq),
      "id" -> str(id),
      "data" -> Null,
      "error" -> str(error)
    ))
    rpcOutBuffer.add((seq, msgJson))
    sendRaw(msgJson)
    seq
  }

  // --- Sending: Events (durable, EventLog seq) ---

  def push(event: Event): Task[Long] = {
    outboundLog.append(channelId, event).map { seq =>
      val eventJson = JsonFormatter.Default(obj(
        "type" -> str("event"),
        "seq" -> num(seq),
        "data" -> event.json
      ))
      sendRaw(eventJson)
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
    // Also detect disconnect via status change (handles abrupt disconnections)
    ws.status.attach {
      case spice.http.ConnectionStatus.Closed => handleDisconnect()
      case _ => // ignore
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
    val msg = JsonFormatter.Default(obj(
      "type" -> str("connect"),
      "clientId" -> str(clientId),
      "info" -> info.json
    ))
    sendRaw(msg)
  }

  def sendResume(clientId: String, lastSeq: Long, info: Info): Unit = {
    _state @= ProtocolState.Handshaking
    val msg = JsonFormatter.Default(obj(
      "type" -> str("resume"),
      "clientId" -> str(clientId),
      "lastSeq" -> num(lastSeq),
      "info" -> info.json
    ))
    sendRaw(msg)
  }

  def sendConnected(lastClientSeq: Long, resumed: Boolean): Unit = {
    val msg = JsonFormatter.Default(obj(
      "type" -> str("connected"),
      "lastClientSeq" -> num(lastClientSeq),
      "resumed" -> bool(resumed)
    ))
    sendRaw(msg)
  }

  def activate(): Unit = {
    _state @= ProtocolState.Active
    lastPongReceived @= System.currentTimeMillis()
    startTimers()
  }

  def replayAfter(seq: Long): Task[Unit] = {
    outboundLog.replay(channelId, seq).map { events =>
      events.foreach { case (eventSeq, event) =>
        sendLogged(eventSeq, event)
      }
    }
  }

  def replayRpcAfter(seq: Long): Unit = {
    rpcOutBuffer.asScala.filter(_._1 > seq).foreach { case (_, json) =>
      sendRaw(json)
    }
  }

  def highestProcessedSeq: Long = tracker.highestProcessedSeq

  def sendAck(): Unit = {
    val seq = tracker.highestProcessedSeq
    if (seq > 0) {
      val msg = JsonFormatter.Default(obj("type" -> str("ack"), "seq" -> num(seq)))
      sendRaw(msg)
      tracker.resetAckCount()
    }
  }

  // --- Internal: message dispatch ---

  private def handleRawMessage(text: String): Unit = {
    val json = JsonParser(text)
    val msgType = json("type").asString

    msgType match {
      case "invoke" =>
        val seq = json("seq").asLong
        if (tracker.acceptInbound(seq)) {
          val msg = InvokeMessage(seq, json("id").asString, json("tool").asString, json("args"))
          onInvoke @= msg
          maybeAck()
        }

      case "result" =>
        val seq = json("seq").asLong
        if (tracker.acceptInbound(seq)) {
          val id = json("id").asString
          val completable = pendingInvocations.remove(id)
          if (completable != null) {
            json.get("error").filter(_ != Null) match {
              case Some(err) => completable.failure(new RuntimeException(err.asString))
              case None => completable.success(json("data"))
            }
          }
          maybeAck()
        }

      case "event" =>
        val seq = json("seq").asLong
        if (tracker.acceptInbound(seq)) {
          val event = json("data").as[Event]
          onEvent @= (seq, event)
          maybeAck()
        }

      case "ack" =>
        val ackedSeq = json("seq").asLong
        remoteAckedSeq @= ackedSeq
        // Remove acked RPC messages from buffer
        rpcOutBuffer.removeIf(_._1 <= ackedSeq)

      case "ping" =>
        val ts = json("ts").asLong
        val pong = JsonFormatter.Default(obj("type" -> str("pong"), "ts" -> num(ts)))
        sendRaw(pong)

      case "pong" =>
        lastPongReceived @= System.currentTimeMillis()

      case "connect" | "resume" | "connected" | "switch" | "switched" =>
        handleHandshakeMessage(json, msgType)

      case "error" =>
        val msg = ErrorMessage(json("code").asString, json("message").asString)
        onError @= msg

      case other =>
        scribe.warn(s"DurableSocket: unknown message type: $other")
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
    val gen = timerGeneration
    heartbeatLoop(gen).start()
    ackTimerLoop(gen).start()
  }

  private def stopTimers(): Unit = {
    timerGeneration += 1
  }

  private def heartbeatLoop(gen: Long): Task[Unit] =
    Task.sleep(config.heartbeatInterval).flatMap { _ =>
      if (_state() == ProtocolState.Active && gen == timerGeneration) {
        val ping = JsonFormatter.Default(obj("type" -> str("ping"), "ts" -> num(System.currentTimeMillis())))
        sendRaw(ping)
        if (System.currentTimeMillis() - lastPongReceived() > config.heartbeatTimeout.toMillis) {
          handleDisconnect()
          Task.unit
        } else {
          heartbeatLoop(gen)
        }
      } else Task.unit
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
