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
import scala.util.{Failure, Success, Try}

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

  /** Await durable receipt before acknowledging the inbound sequence. The handler builds its task at arrival
    * (so it can capture identity); tasks run in wire order. onEvent then observes successful processing.
    * Without a handler, synchronous channel delivery remains unchanged. */
  @volatile var inboundHandler: Option[(Long, Event) => Task[Unit]] = None
  private val inboundLock = new Object
  private var inboundTail: Task[Unit] = Task.unit
  private var pendingInbound = 0

  private val _state: Var[ProtocolState] = Var(ProtocolState.Disconnected)
  val state: Val[ProtocolState] = _state

  // --- Internal state ---
  private val rawWs: Var[Option[WebSocket]] = Var(None)
  private val remoteAckedSeq: Var[Long] = Var(0L)
  private val tracker = new SequenceTracker(config)

  // Replays and live events go out in sequence order: see [[replayAfter]].
  private val delivery = new Object
  private var replaying: Boolean = false
  private var held: Vector[(Long, Event)] = Vector.empty

  /** Re-authorize recorded payloads against the current session identity on every replay. Returning None omits
    * the payload while retaining its sequence watermark. Live delivery remains the sender's responsibility. */
  @volatile var replayTransform: (Id, Event) => Task[Option[Event]] = (_, event) => Task.pure(Some(event))

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
      delivery.synchronized {
        if (replaying) held = held :+ (seq -> event)
        else sendLogged(seq, event)
      }
      seq
    }
  }

  /** Send a logged event appended elsewhere (a [[DurableSocketServer.broadcast]]) if this socket is active. While a
    * replay is under way the event is held and sent after the replayed ones, so the peer never sees a later sequence
    * number before an earlier one (it drops anything at or below the highest it has seen). */
  def deliver(seq: Long, event: Event): Unit = delivery.synchronized {
    if (replaying) held = held :+ (seq -> event)
    else if (_state() == ProtocolState.Active) sendLogged(seq, event)
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

  /** Forget the inbound high-water mark: the peer's numbering has started over. */
  def resetInbound(): Unit = tracker.reset(0L)

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
    // A message part-way through its chunks is lost with the connection; what the peer can read is learnt again.
    partial.clear()
    peerReadsChunks = false
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

  /**
    * Drop the current transport WITHOUT closing the protocol.
    *
    * Identical to [[close]] except the state lands on `Disconnected` rather than `Closed`, leaving the connection
    * eligible to be re-established. [[close]] is terminal by design — `DurableSocketClient.handleDisconnect` refuses
    * to reconnect a `Closed` protocol — which makes it the wrong tool for "drop this socket and re-dial", the case a
    * rolling deploy needs when the instance holding the socket is being retired and wants the client to land on its
    * replacement.
    *
    * Backs [[DurableSocketClient.reconnect]]. No-op once closed.
    */
  def disconnect(): Unit = if (_state() != ProtocolState.Closed) {
    val ws = rawWs()
    unbind()
    // `unbind` only demotes an ACTIVE protocol; a socket dropped mid-handshake
    // would otherwise be stranded in `Handshaking` and never re-dial.
    _state @= ProtocolState.Disconnected
    ws.foreach(_.disconnect())
  }

  // --- Protocol internals ---

  def sendConnect(clientId: String, info: Info): Unit = {
    _state @= ProtocolState.Handshaking
    sendRaw(JsonFormatter.Default(obj(
      "type" -> str("connect"),
      "clientId" -> str(clientId),
      "info" -> info.json,
      "chunks" -> bool(true)
    )))
  }

  def sendResume(clientId: String, lastSeq: Long, info: Info): Unit = {
    _state @= ProtocolState.Handshaking
    sendRaw(JsonFormatter.Default(obj(
      "type" -> str("resume"),
      "clientId" -> str(clientId),
      "lastSeq" -> num(lastSeq),
      "info" -> info.json,
      "chunks" -> bool(true)
    )))
  }

  /** Tell the peer this instance is retiring so it should re-dial onto the replacement (see
    * [[DurableSocketClient.reconnect]]). Application-level, so it works even where the transport can't
    * surface a coded close; a graceful drain follows it with a WS 1001 close for stragglers. */
  def sendGoingAway(reason: String): Unit = {
    sendRaw(JsonFormatter.Default(obj(
      "type" -> str("going-away"),
      "reason" -> str(reason)
    )))
  }

  def sendConnected(lastClientSeq: Long, resumed: Boolean): Unit = {
    sendRaw(JsonFormatter.Default(obj(
      "type" -> str("connected"),
      "lastClientSeq" -> num(lastClientSeq),
      "resumed" -> bool(resumed),
      "chunks" -> bool(true)
    )))
  }

  def activate(): Unit = {
    _state @= ProtocolState.Active
    startTimers()
    if (_files != null) _files.onReactivated()
  }

  /** Resend the logged events after `seq`. Live events are held from this call (not from when the task runs) until
    * the replay has been sent, then follow it in order: without that, an event logged after the replay's snapshot but
    * before the socket was active reached the peer by neither path, and one sent live ahead of the replay made the
    * peer drop the replayed ones below it. */
  def replayAfter(seq: Long): Task[Unit] = {
    delivery.synchronized { replaying = true }
    val channel = channelId
    outboundLog.replay(channel, seq).flatMap { events =>
      replayEvents(channel, events).flatMap(_ => drainReplay(channel, events.lastOption.map(_._1).getOrElse(seq)))
    }.handleError { throwable =>
      // A failed authorization must never flush unfiltered held payloads.
      delivery.synchronized { held = Vector.empty; replaying = false }
      Task.error(throwable)
    }
  }

  private def replayEvents(channel: Id, events: Iterable[(Long, Event)]): Task[Unit] =
    events.foldLeft(Task.unit) { case (previous, (seq, event)) =>
      previous.flatMap(_ => Task.defer(replayTransform(channel, event)).map(_.foreach(sendLogged(seq, _))))
    }

  /** Keep live pushes held until every batch has passed the same authorization as logged replay. */
  private def drainReplay(channel: Id, sent: Long): Task[Unit] = Task.defer {
    val batch = delivery.synchronized {
      val pending = held.filter(_._1 > sent).sortBy(_._1)
      held = Vector.empty
      if (pending.isEmpty) replaying = false
      pending
    }
    if (batch.isEmpty) Task.unit
    else replayEvents(channel, batch).flatMap(_ => drainReplay(channel, batch.last._1))
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

  /** Whether the peer said in its handshake that it reads `chunk` frames: only then is a long message sent as chunks. */
  @volatile private var peerReadsChunks: Boolean = false

  /** Record what the peer's handshake said about reading `chunk` frames. */
  def peerChunks(json: Json): Unit = peerReadsChunks = json.get("chunks").exists {
    case Bool(b, _) => b
    case _ => false
  }

  private val chunkCounter = new AtomicLong(0L)
  /** How many messages this socket has sent as chunks. */
  private[durable] def chunkedMessagesSent: Long = chunkCounter.get()
  // Messages arriving as chunks, by id: the parts received so far and their total length.
  private val partial = new ConcurrentHashMap[String, (Array[String], Long)]()

  private val TypePrefix = """"type"\s*:\s*"([A-Za-z\-]+)"""".r
  private val IdPrefix = """"id"\s*:\s*(\d+)""".r
  private val SeqPrefix = """"seq"\s*:\s*(\d+)""".r

  /**
   * Answer a message that could not be read — too long, or not the JSON it should be — instead of dropping it: a
   * request by its id, so the caller's pending `ask` fails at once, anything else with an `error` frame carrying its
   * `seq` where it had one. What it was is read from its opening, which is all a message too long to parse offers.
   */
  private def refuse(text: String, code: String, message: String): Unit = {
    val head = text.take(512)
    scribe.warn(s"DurableSocket: refused a message of ${text.length} characters ($code): $message")
    (TypePrefix.findFirstMatchIn(head).map(_.group(1)), IdPrefix.findFirstMatchIn(head).map(_.group(1).toLong)) match {
      case (Some("request"), Some(id)) =>
        sendRaw(JsonFormatter.Default(obj("type" -> str("response-error"), "id" -> num(id), "code" -> str(code), "message" -> str(message))))
      case _ =>
        val seq = SeqPrefix.findFirstMatchIn(head).map(m => "seq" -> num(m.group(1).toLong)).toList
        sendRaw(JsonFormatter.Default(obj((List("type" -> str("error"), "code" -> str(code), "message" -> str(message)) ++ seq)*)))
    }
  }

  /** One part of a message sent as chunks; the whole is read once every part has arrived. */
  private def acceptChunk(json: Json): Unit = {
    val id = json("id").asString
    val index = json("index").asInt
    val count = json("count").asInt
    val data = json("data").asString
    val (parts, total) = partial.computeIfAbsent(id, _ => (new Array[String](count), 0L))
    val length = total + data.length
    if (length > config.maxMessageChars) {
      partial.remove(id)
      refuse(data, "message-too-large", s"A message longer than ${config.maxMessageChars} characters is not accepted.")
    } else if (index >= 0 && index < parts.length) {
      parts(index) = data
      partial.put(id, (parts, length))
      if (parts.forall(_ != null)) {
        partial.remove(id)
        handleRawMessage(parts.mkString)
      }
    }
  }

  private def handleRawMessage(text: String): Unit =
    if (text.length > config.maxMessageChars)
      refuse(text, "message-too-large", s"A message longer than ${config.maxMessageChars} characters is not accepted.")
    else Try(JsonParser(text)) match {
      case Failure(t) => refuse(text, "unreadable", s"The message could not be read: ${t.getMessage}")
      case Success(json) => dispatch(json)
    }

  private def enqueueInbound(seq: Long, event: Event, handler: (Long, Event) => Task[Unit]): Unit = {
    val queued = inboundLock.synchronized {
      if (_state() == ProtocolState.Closed || seq <= tracker.highestProcessedSeq) None
      else if (pendingInbound >= config.maxPendingInbound) {
        onError @= ErrorMessage("input-overflow", "Too many input events await durable receipt")
        close()
        None
      } else {
        val done = Task.completable[Unit]
        val previous = inboundTail
        inboundTail = done
        pendingInbound += 1
        // Capture application identity now; effects remain inside the returned task.
        val work = Try(handler(seq, event)).fold(Task.error, identity)
        Some((previous, done, work))
      }
    }
    queued.foreach { (previous, done, work) =>
      previous.flatMap { _ =>
        if (_state() == ProtocolState.Closed || seq <= tracker.highestProcessedSeq) Task.unit
        else work.map { _ =>
          tracker.acceptInbound(seq)
          onEvent @= (seq, event)
          maybeAck()
        }
      }.map(_ => { done.success(()); () }).handleError { error =>
        Task {
          done.failure(error)
          if (_state() != ProtocolState.Closed) {
            onError @= ErrorMessage("input-failed", "Input was not durably accepted")
            scribe.warn(s"DurableSocket: input seq=$seq failed before acknowledgement", error)
            close()
          }
        }
      }.guarantee(Task { inboundLock.synchronized { pendingInbound -= 1 }; () }).startUnit()
    }
  }

  private def dispatch(json: Json): Unit = {
    json.get("type").map(_.asString) match {
      case Some("chunk") =>
        acceptChunk(json)

      case Some("event") =>
        val seq = json("seq").asLong
        val event = json("data").as[Event]
        inboundHandler match {
          case Some(handler) => enqueueInbound(seq, event, handler)
          case None if tracker.acceptInbound(seq) => onEvent @= (seq, event); maybeAck()
          case _ => ()
        }

      case Some("ack") =>
        remoteAckedSeq @= json("seq").asLong

      case Some("connect" | "resume" | "connected" | "switch" | "switched") =>
        handleHandshakeMessage(json, json("type").asString)

      case Some("error") =>
        onError @= ErrorMessage(json("code").asString, json("message").asString)
        // An error arriving DURING a handshake is the answer to the handshake, and the connection is not going to
        // become active. Published alone it went nowhere on a reconnect -- there is no caller waiting on one --
        // and the client sat in `Handshaking` for ever, which is the state everything else is gated on.
        if (_state() == ProtocolState.Handshaking) handleHandshakeError(json)

      case Some("going-away") =>
        handleGoingAway(json)

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

  /** The server refused, or could not answer, a connect or a resume. Whoever is driving the connection decides what
    * to do about it; doing nothing leaves the protocol where it is, which is what used to happen. */
  protected def handleHandshakeError(json: Json): Unit = {}

  /** The server is retiring the instance holding this socket and wants us to re-dial onto its
    * replacement. [[DurableSocketClient]] overrides this to trigger a reconnect; a no-op elsewhere. */
  protected def handleGoingAway(json: Json): Unit = {}

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

  /**
   * Send one message. Longer than [[DurableSocketConfig.maxMessageChars]] it is refused here, where the sender can be
   * told, rather than by a peer that could only drop it; longer than [[DurableSocketConfig.maxFrameChars]], to a peer
   * that reads chunks, it goes as `chunk` frames the peer reassembles before reading — never splitting a surrogate pair.
   */
  protected[durable] def sendRaw(text: String): Unit = {
    if (text.length > config.maxMessageChars)
      throw new IllegalArgumentException(
        s"A message of ${text.length} characters exceeds the ${config.maxMessageChars} a durable socket sends; large files go over the file channel.")
    rawWs().foreach { ws =>
      if (!peerReadsChunks || text.length <= config.maxFrameChars) ws.send.text @= text
      else {
        val id = s"${System.identityHashCode(this).toHexString}-${chunkCounter.incrementAndGet()}"
        val bounds = Iterator.iterate(0) { from =>
          val until = (from + config.maxFrameChars).min(text.length)
          if (until < text.length && Character.isHighSurrogate(text.charAt(until - 1))) until - 1 else until
        }.takeWhile(_ < text.length).toVector :+ text.length
        val parts = bounds.zip(bounds.tail)
        parts.zipWithIndex.foreach { case ((from, until), index) =>
          ws.send.text @= JsonFormatter.Compact(obj(
            "type" -> str("chunk"),
            "id" -> str(id),
            "index" -> num(index),
            "count" -> num(parts.size),
            "data" -> str(text.substring(from, until))
          ))
        }
      }
    }
  }

  protected[durable] def sendBinaryRaw(data: ByteBuffer): Unit = {
    rawWs().foreach(ws => ws.send.binary @= ByteBufferData(data))
  }
}
