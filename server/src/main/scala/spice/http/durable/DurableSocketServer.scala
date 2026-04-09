package spice.http.durable

import fabric.*
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw.*
import rapid.Task
import reactify.Channel
import spice.http.{HttpExchange, WebSocketListener}
import spice.http.server.handler.WebSocketHandler

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

case class DurableSession[Id: RW, Event: RW, Info: RW](
  clientId: String,
  info: Info,
  protocol: DurableSocket[Id, Event, Info],
  listener: reactify.Var[spice.http.WebSocketListener],
  connectedAt: Long = System.currentTimeMillis(),
  @volatile var lastActivityAt: Long = System.currentTimeMillis()
) {
  def channelId: Id = protocol.channelId

  def touch(): Unit = lastActivityAt = System.currentTimeMillis()

  def isConnected: Boolean =
    listener().status() == spice.http.ConnectionStatus.Open
}

class DurableSocketServer[Id: RW, Event: RW, Info: RW](
  config: DurableSocketConfig = DurableSocketConfig(),
  eventLog: EventLog[Id, Event],
  resolveChannel: (String, Info) => Task[Id],
  authorizeSwitch: (DurableSession[Id, Event, Info], Id) => Task[Unit] = (_: DurableSession[Id, Event, Info], _: Id) => Task.unit,
  sessionTimeout: FiniteDuration = 30.minutes
) extends WebSocketHandler {
  private val sessions = new ConcurrentHashMap[String, DurableSession[Id, Event, Info]]()
  private val channelSessions = new ConcurrentHashMap[Id, ConcurrentHashMap[String, DurableSession[Id, Event, Info]]]()

  val onSession: Channel[DurableSession[Id, Event, Info]] = Channel[DurableSession[Id, Event, Info]]

  // --- Session lookup ---

  def session(clientId: String): Option[DurableSession[Id, Event, Info]] =
    Option(sessions.get(clientId))

  def sessionsByChannel(channelId: Id): List[DurableSession[Id, Event, Info]] = {
    val map = channelSessions.get(channelId)
    if (map == null) Nil
    else map.values().asScala.toList
  }

  def activeSessions: List[DurableSession[Id, Event, Info]] =
    sessions.values().asScala.toList

  // --- Session management ---

  def removeSession(clientId: String): Unit = {
    val s = sessions.remove(clientId)
    if (s != null) {
      val channelMap = channelSessions.get(s.channelId)
      if (channelMap != null) channelMap.remove(clientId)
      s.protocol.close()
    }
  }

  def expireStale(): Int = {
    val now = System.currentTimeMillis()
    val cutoff = now - sessionTimeout.toMillis
    var expired = 0
    sessions.forEach { (clientId, session) =>
      if (session.lastActivityAt < cutoff && !session.isConnected) {
        removeSession(clientId)
        expired += 1
      }
    }
    if (expired > 0) scribe.info(s"Expired $expired stale DurableSocket sessions")
    expired
  }

  def startSessionExpiry(checkInterval: FiniteDuration = 1.minute): Unit = {
    expiryLoop(checkInterval).start()
  }

  private def expiryLoop(interval: FiniteDuration): Task[Unit] =
    Task.sleep(interval).flatMap { _ =>
      expireStale()
      expiryLoop(interval)
    }

  // --- Broadcast to a channel ---

  def broadcast(channelId: Id, event: Event): Task[Long] = {
    eventLog.append(channelId, event).map { seq =>
      sessionsByChannel(channelId).foreach { session =>
        if (session.protocol.state() == ProtocolState.Active) {
          session.protocol.sendLogged(seq, event)
        }
      }
      seq
    }
  }

  // --- Connection handling ---

  override def connect(exchange: HttpExchange, listener: WebSocketListener): Task[Unit] = Task {
    listener.receive.text.once({ text =>
      val json = JsonParser(text)
      val msgType = json("type").asString

      msgType match {
        case "connect" =>
          val clientId = json("clientId").asString
          val info = json("info").as[Info]
          resolveChannel(clientId, info).map { channelId =>
            val ds = createSocket(channelId)
            ds.bind(listener)
            val durableSession = DurableSession(clientId, info, ds, reactify.Var(listener))
            registerSession(durableSession)

            ds.sendConnected(0L, resumed = false)
            ds.activate()
            onSession @= durableSession
          }.handleError { throwable =>
            sendError(listener, "auth_failed", throwable)
            Task.unit
          }.start()

        case "resume" =>
          val clientId = json("clientId").asString
          val lastServerSeq = json("lastSeq").asLong
          val info = json("info").as[Info]
          val existing = sessions.get(clientId)
          if (existing != null) {
            resolveChannel(clientId, info).flatMap { _ =>
              existing.protocol.unbind()
              existing.protocol.bind(listener)
              existing.listener @= listener
              existing.touch()
              val lastClientSeq = existing.protocol.highestProcessedSeq
              existing.protocol.sendConnected(lastClientSeq, resumed = true)
              existing.protocol.replayAfter(lastServerSeq).map { _ =>
                existing.protocol.activate()
                onSession @= existing
              }
            }.handleError { throwable =>
              sendError(listener, "auth_failed", throwable)
              Task.unit
            }.start()
          } else {
            resolveChannel(clientId, info).map { channelId =>
              val ds = createSocket(channelId)
              ds.bind(listener)
              val durableSession = DurableSession(clientId, info, ds, reactify.Var(listener))
              registerSession(durableSession)

              ds.sendConnected(0L, resumed = false)
              ds.activate()
              onSession @= durableSession
            }.handleError { throwable =>
              sendError(listener, "auth_failed", throwable)
              Task.unit
            }.start()
          }

        case _ =>
          scribe.warn(s"DurableSocketServer: expected connect/resume, got: $msgType")
      }
    }, _ => true)
  }

  // --- Internal ---

  private def createSocket(channelId: Id): DurableSocket[Id, Event, Info] = {
    val server = this
    new DurableSocket[Id, Event, Info](config, eventLog, channelId) {
      override protected def handleHandshakeMessage(json: Json, msgType: String): Unit = {
        if (msgType == "switch") {
          server.handleSwitch(this, json)
        }
      }
    }
  }

  private def handleSwitch(ds: DurableSocket[Id, Event, Info], json: Json): Unit = {
    val newChannelId = json("channelId").as[Id]
    val lastSeq = json("lastSeq").asLong
    val session = sessions.values().asScala.find(_.protocol eq ds)
    session.foreach { s =>
      authorizeSwitch(s, newChannelId).map { _ =>
        val oldMap = channelSessions.get(s.channelId)
        if (oldMap != null) oldMap.remove(s.clientId)

        ds.updateChannelId(newChannelId)
        val newMap = channelSessions.computeIfAbsent(newChannelId, _ => new ConcurrentHashMap())
        newMap.put(s.clientId, s)
        s.touch()

        val switched = JsonFormatter.Default(obj(
          "type" -> str("switched"),
          "channelId" -> newChannelId.json,
          "lastSeq" -> num(ds.highestProcessedSeq)
        ))
        ds.sendRaw(switched)
        ds.replayAfter(lastSeq).start()
      }.handleError { throwable =>
        val errorMsg = JsonFormatter.Default(obj(
          "type" -> str("error"),
          "code" -> str("access_denied"),
          "message" -> str(Option(throwable.getMessage).getOrElse("Channel access denied"))
        ))
        ds.sendRaw(errorMsg)
        Task.unit
      }.start()
    }
  }

  /** Server-initiated channel switch. Moves a session to a new channel without
    * client negotiation or authorization. Sends "switched" to the client and replays
    * any existing events on the new channel. */
  def serverSwitch(clientId: String, newChannelId: Id): Task[Unit] = {
    session(clientId) match {
      case Some(s) =>
        val ds = s.protocol
        val oldMap = channelSessions.get(s.channelId)
        if (oldMap != null) oldMap.remove(s.clientId)

        ds.updateChannelId(newChannelId)
        val newMap = channelSessions.computeIfAbsent(newChannelId, _ => new ConcurrentHashMap())
        newMap.put(s.clientId, s)
        s.touch()

        val switched = JsonFormatter.Default(obj(
          "type" -> str("switched"),
          "channelId" -> newChannelId.json,
          "lastSeq" -> num(ds.highestProcessedSeq)
        ))
        ds.sendRaw(switched)
        Task.unit
      case None =>
        Task.error(RuntimeException(s"No session found for clientId $clientId"))
    }
  }

  private def registerSession(session: DurableSession[Id, Event, Info]): Unit = {
    sessions.put(session.clientId, session)
    val channelMap = channelSessions.computeIfAbsent(session.channelId, _ => new ConcurrentHashMap())
    channelMap.put(session.clientId, session)
  }

  private def sendError(listener: WebSocketListener, code: String, throwable: Throwable): Unit = {
    val errorMsg = JsonFormatter.Default(obj(
      "type" -> str("error"),
      "code" -> str(code),
      "message" -> str(Option(throwable.getMessage).getOrElse("Connection rejected"))
    ))
    listener.send.text @= errorMsg
    listener.disconnect()
  }
}
