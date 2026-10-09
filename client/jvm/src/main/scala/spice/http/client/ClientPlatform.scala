package spice.http.client

import java.net.{ConnectException, NoRouteToHostException, SocketTimeoutException, UnknownHostException}
import java.net.http.HttpConnectTimeoutException

object ClientPlatform {
  def defaultSaveDirectory: String = System.getProperty("java.io.tmpdir")

  /** Whether `throwable`, or any cause of it, says the connection was never made, so the server cannot have received
    * the request. A connect timeout from a plain socket carries no type of its own, only the JDK's message. */
  def failedBeforeConnect(throwable: Throwable): Boolean =
    Iterator.iterate(throwable)(_.getCause).takeWhile(_ != null).take(16).exists {
      case _: ConnectException | _: UnknownHostException | _: NoRouteToHostException => true
      case _: HttpConnectTimeoutException => true
      case t: SocketTimeoutException => Option(t.getMessage).exists(_.toLowerCase.contains("connect timed out"))
      case _ => false
    }
}
