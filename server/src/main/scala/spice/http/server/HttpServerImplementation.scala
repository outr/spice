package spice.http.server

import cats.effect.IO

trait HttpServerImplementation {
  def isRunning: Boolean

  def start(server: HttpServer): IO[Unit]

  def stop(server: HttpServer): IO[Unit]
}
