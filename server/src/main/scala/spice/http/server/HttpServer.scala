package spice.http.server

import rapid.Task
import scribe.{rapid => logger}
import spice.http.HttpExchange
import spice.http.server.config.{ServerConfig, ServerSocketListener}
import spice.http.server.handler.{HttpHandler, LifecycleHandler}
import spice.net.URLPath
import spice.store.Store
import spice.{ImplementationManager, Initializable}

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.concurrent.{Executors, ThreadFactory}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait HttpServer extends LifecycleHandler with Initializable {
  val config = new ServerConfig(this)

  private val implementation = HttpServerImplementationManager(this)

  def isRunning: Boolean = isInitialized && implementation.isRunning

  /**
   * Init is called on start(), but only the first time. If the server is restarted it is not invoked again.
   */
  override protected def initialize(): Task[Unit] = Task.unit

  def start(): Task[Unit] = for {
    _ <- init()
    _ <- implementation.start(this)
    _ <- logger.info(s"Server started on ${config.enabledListeners.mkString(", ")}")
  } yield {
    ()
  }

  def errorRecovery(exchange: HttpExchange, throwable: Throwable): Task[HttpExchange] = logger
    .error(throwable)
    .map(_ => exchange)

  def stop(): Task[Unit] = implementation.stop(this)

  def restart(): Unit = synchronized {
    stop()
    start()
  }

  override protected def preHandle(exchange: HttpExchange): Task[HttpExchange] = Task {
    val listener = config.listeners().find(l => l.matches(exchange.request.url))
    listener.map { l =>
      ServerSocketListener.set(exchange, l)
      BasePath.set(exchange, l.basePath)
      BaseURL.set(exchange, l.baseUrlFor(exchange.request.url).get)
      exchange.copy(
        path = URLPath(exchange.path.parts.drop(l.basePath.parts.length))
      )
    }.getOrElse(exchange)
  }

  override protected def postHandle(exchange: HttpExchange): Task[HttpExchange] = Task.pure(exchange)

  def whileRunning(delay: FiniteDuration = 1.second): Task[Unit] = if (isRunning) {
    Task.sleep(delay).flatMap(_ => whileRunning(delay))
  } else {
    Task.unit
  }

  def dispose(): Task[Unit] = stop()
}