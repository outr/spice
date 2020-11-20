package spice.server

import moduload.Moduload
import spice.server.handler.HttpHandler

import scribe.Execution.global

case class Server(handler: HttpHandler, implementation: ServerImplementation = Server.DefaultImplementation)

object Server {
  lazy val DefaultImplementation: ServerImplementation = {
    Moduload.load()
    ???
  }
}