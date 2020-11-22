package spice.server

import moduload.Moduload
import spice.server.handler.HttpHandler

import scribe.Execution.global

class Server(val handler: HttpHandler)
            (implicit val implementation: ServerImplementation = Server.DefaultImplementation)

object Server {
  lazy val DefaultImplementation: ServerImplementation = {
    Moduload.load()
    ???
  }
}