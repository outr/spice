package spice.api.server

import fabric.{Json, obj, str}
import fabric.io.JsonFormatter
import fabric.rw.*
import rapid.Task
import spice.http.server.MutableHttpServer
import spice.net.URLPath

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.util.concurrent.ConcurrentHashMap

object WsServerRuntime {
  private val managers = new ConcurrentHashMap[String, WsConnectionManager]()
  private val routedManagers = new ConcurrentHashMap[String, RoutedWebSocketManager]()

  def register(server: MutableHttpServer, basePath: URLPath): Unit = {
    managers.computeIfAbsent(basePath.encoded, _ => new WsConnectionManager(server, basePath))
  }

  def registerRouted(server: MutableHttpServer, basePath: URLPath): Unit = {
    routedManagers.computeIfAbsent(basePath.encoded, _ => new RoutedWebSocketManager(server, basePath))
  }

  def broadcast(basePath: URLPath, message: String): Unit = {
    val manager = managers.get(basePath.encoded)
    if (manager != null) {
      manager.broadcast(message)
    }
  }

  def send(basePath: URLPath, routingKey: String, message: String): Unit = {
    val manager = routedManagers.get(basePath.encoded)
    if (manager != null) {
      manager.send(routingKey, message)
    }
  }

  def createProxy[T](clazz: Class[?], basePath: URLPath, serializers: Map[String, (Array[AnyRef], List[String]) => Json], routingKeyIndex: Int): T = {
    val handler = new InvocationHandler {
      override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
        val methodName = method.getName
        serializers.get(methodName) match {
          case Some(serializer) =>
            val actualArgs = if (args == null) Array.empty[AnyRef] else args
            val argsJson = serializer(actualArgs, Nil)
            val envelope = obj(
              "method" -> str(methodName),
              "args" -> argsJson
            )
            val message = JsonFormatter.Default(envelope)
            if (routingKeyIndex >= 0 && actualArgs.length > routingKeyIndex) {
              val routingKey = actualArgs(routingKeyIndex).toString
              send(basePath, routingKey, message)
            } else {
              broadcast(basePath, message)
            }
            Task.pure(())
          case None =>
            // Default Object methods
            method.getName match {
              case "toString" => s"WsServerProxy(${basePath.encoded})"
              case "hashCode" => Int.box(basePath.hashCode())
              case "equals" => Boolean.box(false)
              case _ => throw new UnsupportedOperationException(s"Unknown method: $methodName")
            }
        }
      }
    }
    Proxy.newProxyInstance(clazz.getClassLoader, Array(clazz), handler).asInstanceOf[T]
  }
}
