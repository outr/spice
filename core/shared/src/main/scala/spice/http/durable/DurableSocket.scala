package spice.http.durable

import fabric.*
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw.*
import rapid.Task
import reactify.{Channel, Val, Var}
import spice.http.{ByteBufferData, WebSocket}

import java.nio.ByteBuffer
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.*

class DurableSocket[Id: RW, Event: RW, Info: RW](
  val config: DurableSocketConfig,
  val outboundLog: EventLog[Id, Event],
  initialChannelId: Id,
  val fileTransfer: FileTransferConfig = FileTransferConfig()
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

  // --- RPC (request/response) facet: ephemeral, correlated by id ---
  private val rpcCounter = new AtomicLong(0L)
  private val pendingRequests = new ConcurrentHashMap[Long, rapid.task.Completable[Json]]()

  /** Handles an inbound RPC request (request payload JSON -> response payload JSON). The owner — a
    * [[DurableSocketServer]] session for client->server RPC, or the client itself for server->client
    * RPC — installs this; the default rejects every request. Throw an [[RpcException]] to send a
    * coded error back to the caller. */
  @volatile var requestHandler: Json => Task[Json] = _ =>
    Task.error(new RpcException("no_handler", "No RPC request handler registered"))

  // --- File transfer facet (one payload type per socket) ---
  @volatile private var _files: FileChannel[?] = null

  /** Typed file-transfer facet. The first call fixes the payload type `F` for this socket;
    * subsequent calls must use the same `F`. See [[FileChannel]]. */
  def files[F: RW]: FileChannel[F] = synchronized {
    if (_files == null) _files = new FileChannel[F](this, fileTransfer)
    _files.asInstanceOf[FileChannel[F]]
  }

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

  // --- Sending: RPC requests ---

  /** Send a typed request and complete with the peer's typed response. Request and response ride the
    * ephemeral plane (never logged or replayed), so a request issued to a dead connection simply
    * fails on `timeout`. Payloads are any RW values; pairing the request type with a polymorphic
    * Event hierarchy lets its discriminator pick the server handler — no method name on the wire. A
    * peer error arrives as a failed [[RpcException]] carrying the handler's `code`. */
  def ask[Req: RW, Res: RW](request: Req, timeout: FiniteDuration = 30.seconds): Task[Res] = Task.defer {
    val id = rpcCounter.incrementAndGet()
    val completable = Task.completable[Json]
    pendingRequests.put(id, completable)
    sendRaw(JsonFormatter.Default(obj(
      "type" -> str("request"),
      "id" -> num(id),
      "data" -> request.json
    )))
    completable
      .timeout(timeout)
      .map(_.as[Res])
      .guarantee(Task { pendingRequests.remove(id); () })
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
    ws.receive.binary.attach {
      case ByteBufferData(bb) => if (_files != null) _files.acceptChunk(bb)
      case _                  =>
    }
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
    if (_files != null) _files.onReactivated()
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

      case Some("request") =>
        val id = json("id").asLong
        val data = json("data")
        requestHandler(data).map { result =>
          sendRaw(JsonFormatter.Default(obj("type" -> str("response"), "id" -> num(id), "data" -> result)))
        }.handleError { throwable =>
          val (code, message) = throwable match {
            case e: RpcException => (e.code, Option(e.getMessage).getOrElse("RPC error"))
            case other          => ("error", Option(other.getMessage).getOrElse("RPC error"))
          }
          sendRaw(JsonFormatter.Default(obj("type" -> str("response-error"), "id" -> num(id), "code" -> str(code), "message" -> str(message))))
          Task.unit
        }.start()

      case Some("response") =>
        val c = pendingRequests.remove(json("id").asLong)
        if (c != null) c.success(json("data"))

      case Some("response-error") =>
        val c = pendingRequests.remove(json("id").asLong)
        if (c != null) c.failure(new RpcException(json("code").asString, json("message").asString))

      case Some("file-start")  => if (_files != null) _files.acceptStart(json)
      case Some("file-end")    => if (_files != null) _files.acceptEnd(json)
      case Some("file-ack")    => if (_files != null) _files.acceptAck(json)
      case Some("file-resume") => if (_files != null) _files.acceptResume(json)
      case Some("file-abort")  => if (_files != null) _files.acceptAbort(json)

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

  protected[durable] def sendBinaryRaw(data: ByteBuffer): Unit = {
    rawWs().foreach(ws => ws.send.binary @= ByteBufferData(data))
  }
}
