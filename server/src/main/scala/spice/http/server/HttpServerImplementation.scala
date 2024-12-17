package spice.http.server

import rapid.Task

trait HttpServerImplementation {
  def isRunning: Boolean

  def start(server: HttpServer): Task[Unit]

  def stop(server: HttpServer): Task[Unit]
}
