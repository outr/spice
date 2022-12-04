package spice.http.server.openapi.server

import spice.net.Path

trait Service {
  val path: Path

  val get: ServiceCall = ServiceCall.NotSupported
  val post: ServiceCall = ServiceCall.NotSupported
  val put: ServiceCall = ServiceCall.NotSupported
}
