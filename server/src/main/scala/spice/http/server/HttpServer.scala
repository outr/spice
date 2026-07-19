package spice.http.server

import rapid.*
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

  // Graceful-drain hooks, run in registration order when the server stops (before the listeners are torn
  // down). Used to drain WebSocket sessions on a rolling deploy. See `onDrain` / `stop`.
  private val drainHandlers = new java.util.concurrent.CopyOnWriteArrayList[() => Task[Unit]]()
  private val shutdownHookInstalled = new java.util.concurrent.atomic.AtomicBoolean(false)

  def start(): Task[Unit] = for {
    _ <- init()
    _ <- implementation.start(this)
    _ <- Task(installShutdownHook())
    _ <- logger.info(s"Server started on ${config.enabledListeners.mkString(", ")}")
  } yield {
    ()
  }

  /** Register work to run when the server is stopping, before the listeners are torn down (e.g. a
    * WebSocket drain on a rolling deploy). Handlers run in registration order; a failing one is logged
    * and does not block the rest, nor the listener shutdown. */
  def onDrain(handler: () => Task[Unit]): Unit = drainHandlers.add(handler)

  private def runDrain(): Task[Unit] = {
    import scala.jdk.CollectionConverters.*
    drainHandlers.asScala.toList.foldLeft(Task.unit) { (acc, handler) =>
      acc.flatMap(_ => handler().handleError(t => Task(scribe.error("Drain handler failed", t))))
    }
  }

  // Drain and stop cleanly on SIGTERM (container stop / rolling deploy) instead of the JVM dying with
  // sockets mid-flight. Installed once, after the first successful start.
  private def installShutdownHook(): Unit = if (shutdownHookInstalled.compareAndSet(false, true)) {
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      try stop().sync()
      catch { case t: Throwable => scribe.error("Error during shutdown-hook stop", t) }
    }, "spice-shutdown"))
  }

  def errorRecovery(exchange: HttpExchange, throwable: Throwable): Task[HttpExchange] = logger
    .error(throwable)
    .map(_ => exchange)

  def stop(): Task[Unit] = runDrain().flatMap(_ => implementation.stop(this))

  def restart(): Task[Unit] = stop().flatMap(_ => start())

  /** Hot-swap the running HTTPS listeners' certificates from disk without dropping
    * connections. New TLS handshakes pick up the fresh cert; in-flight TLS sessions
    * keep their handshake-derived keys. Falls back to `restart()` for server impls
    * that haven't wired hot-reload (see [[HttpServerImplementation.reloadCertificates]]). */
  def reloadCertificates(): Task[Unit] = implementation.reloadCertificates(this)

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