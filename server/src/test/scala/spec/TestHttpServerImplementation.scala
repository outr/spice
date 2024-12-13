package spec

import rapid._
import moduload.Moduload
import spice.http.server.{HttpServer, HttpServerImplementation, HttpServerImplementationManager}

class TestHttpServerImplementation(server: HttpServer) extends HttpServerImplementation {
  private var running = false

  override def isRunning: Boolean = running

  override def start(server: HttpServer): Task[Unit] = {
    running = true
    IO.unit
  }

  override def stop(server: HttpServer): Task[Unit] = {
    running = false
    IO.unit
  }
}

object TestHttpServerImplementation extends Moduload {
  override def load(): Unit = {
    HttpServerImplementationManager.register(new TestHttpServerImplementation(_))
  }

  override def error(t: Throwable): Unit = {
    scribe.error("Error while attempting to register TestHttpServerImplementation", t)
  }
}