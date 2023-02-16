package spice.http.server

import cats.effect.IO

import java.net.{BindException, InetAddress, ServerSocket}
import scribe.cats.{io => logger}
import spice.net.IP

object ServerUtil {
  def isPortAvailable(port: Int, host: String = "127.0.0.1"): IO[Boolean] = findAvailablePort(List(port), host)
    .map(_.isDefined)

  def findAvailablePort(ports: Seq[Int] = List(0), host: String = "127.0.0.1"): IO[Option[Int]] = if (ports.isEmpty) {
    IO.pure(None)
  } else {
    val io = IO {
      val port = ports.head
      scribe.info(s"Attempting port: $port")
      val ss = new ServerSocket(port, 50, InetAddress.getByName(host))
      try {
        Some(ss.getLocalPort)
      } finally {
        ss.close()
      }
    }
    io.redeemWith({
      case exc: BindException if exc.getMessage.startsWith("Address already in use") =>
        findAvailablePort(ports.tail, host)
      case exc =>
        logger.error(s"Error occurred attempting to bind to $host:${ports.head}", exc).map(_ => None)
    }, IO.pure)
  }

  def localIPs(): List[IP] = {
    val localhost = InetAddress.getLocalHost.getCanonicalHostName
    val addresses = InetAddress.getAllByName(localhost)
    addresses.toList.flatMap(a => IP.fromString(a.getHostAddress))
  }
}