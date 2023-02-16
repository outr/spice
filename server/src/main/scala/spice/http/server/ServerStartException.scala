package spice.http.server

import spice.http.server.config.ServerSocketListener

case class ServerStartException(message: String,
                                listeners: List[ServerSocketListener],
                                cause: Throwable) extends RuntimeException(s"$message (${listeners.mkString(" ")})", cause)
