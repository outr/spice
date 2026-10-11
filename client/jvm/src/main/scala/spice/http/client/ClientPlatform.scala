package spice.http.client

import java.net.{ConnectException, NoRouteToHostException, SocketTimeoutException, UnknownHostException}

object ClientPlatform {
  def defaultSaveDirectory: String = System.getProperty("java.io.tmpdir")

  /** Matched by name: `java.net.http` is absent on Android, which shares this module, so a class reference would fail
    * there at link time. */
  private val HttpConnectTimeoutName = "java.net.http.HttpConnectTimeoutException"

  /** Whether `throwable`, or any cause of it, says the connection was never made, so the server cannot have received
    * the request. A connect timeout from a plain socket carries no type of its own, only the JDK's message. */
  def failedBeforeConnect(throwable: Throwable): Boolean =
    Iterator.iterate(throwable)(_.getCause).takeWhile(_ != null).take(16).exists {
      case _: ConnectException | _: UnknownHostException | _: NoRouteToHostException => true
      case t if t.getClass.getName == HttpConnectTimeoutName => true
      case t: SocketTimeoutException => Option(t.getMessage).exists(_.toLowerCase.contains("connect timed out"))
      case _ => false
    }
}
