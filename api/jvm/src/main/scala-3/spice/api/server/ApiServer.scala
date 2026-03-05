package spice.api.server

import spice.http.server.MutableHttpServer
import spice.net.URLPath

object ApiServer {
  inline def mount[T](impl: T, server: MutableHttpServer, basePath: URLPath): Unit =
    ${ ApiServerMacro.mount[T]('impl, 'server, 'basePath) }
}
