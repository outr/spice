package spice.http.server

import rapid.*

import java.net.{BindException, InetAddress, ServerSocket}
import spice.net.IP

object ServerUtil {
  def isPortAvailable(port: Int, host: String = "127.0.0.1"): Task[Boolean] = findAvailablePort(List(port), host)
    .map(_.isDefined)

  def findAvailablePort(ports: Seq[Int] = List(0), host: String = "127.0.0.1"): Task[Option[Int]] = if (ports.isEmpty) {
    Task.pure(None)
  } else {
    val task = Task[Option[Int]] {
      val port = ports.head
      scribe.info(s"Attempting port: $port")
      val ss = new ServerSocket(port, 50, InetAddress.getByName(host))
      try {
        Some(ss.getLocalPort)
      } finally {
        ss.close()
      }
    }
    task.handleError {
      case exc: BindException if exc.getMessage.startsWith("Address already in use") =>
        findAvailablePort(ports.tail, host)
      case exc => logger.error(s"Error occurred attempting to bind to $host:${ports.head}", exc).map(_ => None)
    }
  }

  def localIPs(): List[IP] = {
    val localhost = InetAddress.getLocalHost.getCanonicalHostName
    val addresses = InetAddress.getAllByName(localhost)
    addresses.toList.flatMap(a => IP.fromString(a.getHostAddress))
  }
}