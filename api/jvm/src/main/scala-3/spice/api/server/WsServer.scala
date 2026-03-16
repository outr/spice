package spice.api.server

import spice.http.server.MutableHttpServer
import spice.net.URLPath

object WsServer {
  inline def derive[T](server: MutableHttpServer, basePath: URLPath): T =
    ${ WsServerMacro.derive[T]('server, 'basePath, '{ -1 }) }

  inline def deriveRouted[T](server: MutableHttpServer, basePath: URLPath): T =
    ${ WsServerMacro.derive[T]('server, 'basePath, '{ 0 }) }
}
