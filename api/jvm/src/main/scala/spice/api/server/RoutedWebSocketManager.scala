package spice.api.server

import rapid.Task
import scribe.mdc.MDC
import spice.http.{HttpExchange, WebSocketListener}
import spice.http.content.Content
import spice.http.server.MutableHttpServer
import spice.http.server.handler.{HttpHandler, WebSocketHandler}
import spice.net.{ContentType, URL, URLPath}

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.jdk.CollectionConverters.*

class RoutedWebSocketManager(server: MutableHttpServer, basePath: URLPath) {
  // routingKey -> listeners
  private val registered = new ConcurrentHashMap[String, ConcurrentLinkedQueue[WebSocketListener]]()
  // Messages buffered before any listener registered for that key
  private val pending = new ConcurrentHashMap[String, ConcurrentLinkedQueue[(String, Long)]]()
  // Single-use tokens: token -> routingKey
  private val tokens = new ConcurrentHashMap[String, String]()

  private val PendingMaxAgeMs = 30_000L
  private val tokenPath = URLPath.parse(s"${basePath.encoded}/token")

  // Register token HTTP endpoint
  server.handler
    .matcher((url: URL) => url.path == tokenPath)
    .handle { exchange =>
      val routingKey = exchange.request.url.parameters.value("routingKey")
      routingKey match {
        case Some(key) =>
          val token = java.util.UUID.randomUUID().toString
          tokens.put(token, key)
          exchange.withContent(
            Content.string(s"""{"token":"$token"}""", ContentType.`application/json`)
          )
        case None =>
          exchange.withContent(
            Content.string("""{"error":"routingKey parameter required"}""", ContentType.`application/json`)
          )
      }
    }

  // Register WebSocket handler
  server.handler
    .matcher((url: URL) => url.path == basePath)
    .wrap(new WebSocketHandler {
      override def connect(exchange: HttpExchange, listener: WebSocketListener): Task[Unit] = Task {
        val tokenOpt = exchange.request.url.parameters.value("token")
        tokenOpt.flatMap(t => Option(tokens.remove(t))) match {
          case Some(routingKey) =>
            val queue = registered.computeIfAbsent(routingKey, _ => new ConcurrentLinkedQueue[WebSocketListener]())
            queue.add(listener)
            scribe.info(s"WS client connected at $basePath for key=$routingKey (listeners: ${queue.size()})")

            // Flush pending messages
            val pendingQueue = pending.get(routingKey)
            if (pendingQueue != null) {
              var entry = pendingQueue.poll()
              while (entry != null) {
                listener.send.text @= entry._1
                entry = pendingQueue.poll()
              }
              pending.remove(routingKey)
            }

            // Cleanup on disconnect
            listener.receive.close.attach { _ =>
              queue.remove(listener)
              scribe.info(s"WS client disconnected from $basePath for key=$routingKey (listeners: ${queue.size()})")
              if (queue.isEmpty) {
                registered.remove(routingKey, queue)
              }
            }
          case None =>
            scribe.warn(s"WS connection at $basePath with invalid/missing token, ignoring")
        }
      }
    })

  def send(routingKey: String, message: String): Unit = {
    val queue = registered.get(routingKey)
    if (queue != null && !queue.isEmpty) {
      queue.forEach { listener =>
        listener.send.text @= message
      }
    } else {
      // Buffer the message
      val pendingQueue = pending.computeIfAbsent(routingKey, _ => new ConcurrentLinkedQueue[(String, Long)]())
      val now = System.currentTimeMillis()
      pendingQueue.add((message, now))
      // Evict old entries
      pendingQueue.removeIf(_._2 < now - PendingMaxAgeMs)
    }
  }
}
