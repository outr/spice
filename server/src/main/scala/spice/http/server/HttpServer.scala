package spice.http.server

import cats.effect.IO
import scribe.cats.{io => logger}

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait HttpServer extends HttpHandler {
  val config = new ServerConfig(this)

  private lazy val implementation = HttpServer.implementation
  private val initialized = new AtomicBoolean(false)

  def isInitialized: Boolean = initialized.get()
  def isRunning: Boolean = isInitialized && implementation.isRunning

  def initialize(): IO[Unit] = {
    val shouldInit = initialized.compareAndSet(false, true)
    if (shouldInit) {
      init()
    } else {
      IO.unit
    }
  }

  /**
   * Init is called on start(), but only the first time. If the server is restarted it is not invoked again.
   */
  protected def init(): IO[Unit] = IO.unit

  def start(): IO[Unit] = for {
    _ <- initialize()
    _ <- implementation.start(this)
    _ <- logger.info(s"Server started on ${config.enabledListeners.mkString(", ")}")
  } yield {
    ()
  }

  def stop(): IO[Unit] = implementation.stop(this)

  def restart(): Unit = synchronized {
    stop()
    start()
  }

  def whileRunning(delay: FiniteDuration = 1.second): IO[Unit] = if (isRunning) {
    IO.sleep(delay).flatMap(_ => whileRunning(delay))
  } else {
    IO.unit
  }

  def dispose(): Unit = stop()
}

object HttpServer {
  private var _implementation: Option[HttpServerImplementation] = None

  def implementation: HttpServerImplementation = _implementation
    .getOrElse(throw new RuntimeException("No HttpServerImplementation registered!"))

  def register(implementation: HttpServerImplementation): Unit = synchronized {
    if (this._implementation.nonEmpty) {
      throw new RuntimeException("HttpServerImplementation already registered!")
    }
    this._implementation = Some(implementation)
  }
}

trait HttpServerImplementation {
  def isRunning: Boolean

  def start(server: HttpServer): IO[Unit]
  def stop(server: HttpServer): IO[Unit]
}