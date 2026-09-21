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
  clientId: String = UUID.randomUUID().toString,
  fileTransfer: FileTransferConfig = FileTransferConfig()
) {
  private val channelSeqs = new ConcurrentHashMap[Id, Long]()

  val protocol: DurableSocket[Id, Event, Info] = new DurableSocket[Id, Event, Info](config, outboundLog, initialChannelId, fileTransfer) {
    override protected def handleHandshakeMessage(json: Json, msgType: String): Unit = {
      msgType match {
        case "connected" =>
          val lastClientSeq = json("lastClientSeq").asLong
          val resumed = json("resumed").asBoolean
          handleConnectedResponse(lastClientSeq, resumed)
        case "switched" =>
          val newChannelId = json("channelId").as[Id]
          val lastSeq = json("lastSeq").asLong
          channelSeqs.put(channelId, highestProcessedSeq)
          handleSwitchComplete(newChannelId, lastSeq)
        case _ =>
      }
    }

    override protected def handleDisconnect(): Unit = {
      super.handleDisconnect()
      if (protocol.state() != ProtocolState.Closed) {
        beginReconnect()
      }
    }

    override protected def handleHandshakeError(json: Json): Unit = {
      scribe.warn("DurableSocket: the server refused the handshake (" +
        json.get("code").map(_.toString).getOrElse("?") + "); re-dialling")
      DurableSocketClient.this.handshakeFailed()
    }

    override protected def handleGoingAway(json: Json): Unit = {
      scribe.info("DurableSocket: server going away; re-dialing onto its replacement")
      DurableSocketClient.this.reconnect()
    }
  }

  val reconnectAttempt: Var[Int] = Var(0)

  // Guards the reconnect loop so at most one runs at a time. `bind` attaches
  // `handleDisconnect` to BOTH `ws.receive.close` AND `ws.status`, so a single
  // socket closing can invoke it twice — which previously started two
  // independent reconnect loops racing to bind two sockets under one clientId,
  // leaving a half-open zombie that still reports as connected.
  private val reconnecting = new java.util.concurrent.atomic.AtomicBoolean(false)

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

  /**
    * Force a reconnect: drop the current transport and re-dial (resuming the session) using the configured
    * `reconnectStrategy`.
    *
    * This is the primitive for "the server asked us to move" — a rolling deploy where the instance holding this
    * socket is being retired and wants the client to land on its replacement, rather than riding a doomed connection
    * until it dies. [[close]] cannot serve that role: it is terminal (it parks the protocol in `Closed`, which
    * permanently disables the reconnect path), so a client that calls it goes dark until its process restarts.
    *
    * The re-dial is driven explicitly rather than by waiting on the dropped socket's close event, because a half-open
    * socket may never deliver one. Because this is an intentional move rather than a failure, the attempt counter is
    * reset so we re-dial at the strategy's base delay instead of an accumulated backoff.
    *
    * No-op once [[close]]d.
    */
  def reconnect(): Unit = if (protocol.state() != ProtocolState.Closed) {
    reconnectAttempt @= 0
    protocol.disconnect()
    beginReconnect()
  }

  def push(event: Event): Task[Long] = protocol.push(event)
  def sendEphemeral(json: Json): Unit = protocol.sendEphemeral(json)

  /** Send a typed RPC request to the server and complete with its typed response. See [[DurableSocket.ask]]. */
  def ask[Req: RW, Res: RW](request: Req, timeout: FiniteDuration = 30.seconds): Task[Res] = protocol.ask[Req, Res](request, timeout)

  /** Install a handler for server->client RPC requests (request payload JSON -> response payload JSON). */
  def requestHandler_=(handler: Json => Task[Json]): Unit = protocol.requestHandler = handler
  def requestHandler: Json => Task[Json] = protocol.requestHandler

  /** Typed file-transfer facet for this connection. See [[FileChannel]]. */
  def files[F: RW]: FileChannel[F] = protocol.files[F]

  /** Fires on every completed handshake with whether the server resumed the session. `false` after an
    * earlier connection means the server no longer held it (a restart, a deploy, an expiry): the new
    * session starts empty, so anything derived from the previous one is stale and should be resynced. */
  val onConnected: Channel[Boolean] = Channel[Boolean]

  val onEvent: Channel[(Long, Event)] = protocol.onEvent
  val onEphemeral: Channel[Json] = protocol.onEphemeral
  val state: Val[ProtocolState] = protocol.state

  def switch(newChannelId: Id): Task[Unit] = {
    val lastSeq = channelSeqs.getOrDefault(newChannelId, 0L)
    protocol.switch(newChannelId, lastSeq)
  }

  private def handleConnectedResponse(lastClientSeq: Long, resumed: Boolean): Unit = {
    if (resumed) {
      protocol.replayAfter(lastClientSeq).start()
    } else {
      // A fresh session numbers its events from the start again; a high-water mark kept from an
      // earlier session would drop every one of them until the new numbering passed it.
      protocol.resetInbound()
      channelSeqs.clear()
    }
    reconnectAttempt @= 0
    reconnecting.set(false)
    protocol.activate()
    onConnected @= resumed
  }

  /** Single entry point into the reconnect loop. Collapses concurrent triggers
    * (duplicate disconnect events, or an explicit [[reconnect]] racing one)
    * into a single loop. */
  private def beginReconnect(): Unit = if (reconnecting.compareAndSet(false, true)) {
    attemptReconnect()
  }

  private def attemptReconnect(): Unit = {
    val attempt = reconnectAttempt()
    config.reconnectStrategy.nextDelay(attempt) match {
      case None =>
        reconnecting.set(false)
        protocol.close()
      case Some(delay) =>
        reconnectAttempt @= attempt + 1
        Task.sleep(delay).flatMap { _ =>
          if (protocol.state() == ProtocolState.Closed) {
            reconnecting.set(false)
            Task.unit
          }
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

  /** Wait for the handshake to be answered, but not for ever.
    *
    * A socket that opens and is then never answered leaves nothing to report and nothing to close: the transport
    * is up, so no error arrives and no disconnect fires, and without a deadline this waited in `Handshaking`
    * until the process restarted -- with everything gated on `Active` quietly taking some other path meanwhile.
    * On the deadline the connection is given up on and re-dialled, which is what every other failure here does. */
  private def waitForActive(waited: FiniteDuration = Duration.Zero): Task[Unit] =
    if (protocol.state() == ProtocolState.Active) Task.unit
    else if (protocol.state() == ProtocolState.Closed) Task.unit
    else if (waited >= config.handshakeTimeout) {
      scribe.warn("DurableSocket: no answer to the handshake in " + config.handshakeTimeout.toString + "; re-dialling")
      handshakeFailed()
      Task.unit
    }
    else Task.sleep(50.millis).flatMap(_ => waitForActive(waited + 50.millis))

  /** A handshake that will not complete: drop the transport and try again.
    *
    * It continues the reconnect loop rather than starting one where a loop is already running. `beginReconnect`
    * collapses concurrent triggers by refusing to start a second loop, which is right for duplicate disconnect
    * events and exactly wrong here -- a handshake failing INSIDE the loop would find the guard already taken and
    * stop the loop dead, which is how a client ended up stranded for good rather than for one attempt. */
  private def handshakeFailed(): Unit = if (protocol.state() != ProtocolState.Closed) {
    protocol.disconnect()
    if (reconnecting.get()) attemptReconnect() else beginReconnect()
  }
}
