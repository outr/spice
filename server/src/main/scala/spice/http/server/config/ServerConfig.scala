package spice.http.server.config

import cats.effect.unsafe.IORuntimeConfig
import fabric.rw._
import profig._
import reactify._
import spice.http.cookie.SameSite
import spice.http.server.{HttpServer, ServerUtil}
import spice.net.{Path, interpolation}

import java.io.File
import java.util.concurrent.TimeUnit

class ServerConfig(server: HttpServer) {
  /**
    * The Server name set in the HTTP header
    */
  val name: Var[String] = Var(Profig("server.name").asOr("spice"))
  /**
   * Enables HTTP/2 support for the server. Defaults to true.
   */
  val enableHTTP2: Var[Boolean] = Var(Profig("server.enableHTTP2").asOr(true))
  /**
   * Enables reuse of connections. Defaults to true.
   */
  val persistentConnections: Var[Boolean] = Var(Profig("server.persistentConnections").asOr(true))
  val webSocketCompression: Var[Boolean] = Var(Profig("server.webSocketCompression").asOr(true))

  val ioRuntimeConfig: Var[IORuntimeConfig] = Var(IORuntimeConfig())

  object session {
    private val config = Profig("session").as[SessionConfig]

    val name: Var[String] = Var(config.name)
    val maxAge: Var[Long] = Var(config.maxAge)
    val domain: Var[Option[String]] = Var(config.domain)
    val path: Var[Option[String]] = Var(config.path)
    val secure: Var[Boolean] = Var(config.secure)
    /**
      * If true, will send secure even over insecure connections. Useful when a higher-level proxy is providing SSL.
      */
    val forceSecure: Var[Boolean] = Var(config.forceSecure)
    val httpOnly: Var[Boolean] = Var(config.httpOnly)
    val sameSite: Var[SameSite] = Var(config.sameSite.toLowerCase match {
      case "normal" => SameSite.Normal
      case "lax" => SameSite.Lax
      case "strict" => SameSite.Strict
    })

    case class SessionConfig(name: String = server.getClass.getSimpleName.replace("$", ""),
                             maxAge: Long = TimeUnit.DAYS.toSeconds(365L),
                             domain: Option[String] = None,
                             path: Option[String] = Some("/"),
                             secure: Boolean = false,
                             forceSecure: Boolean = false,
                             httpOnly: Boolean = true,
                             sameSite: String = "strict")

    object SessionConfig {
      implicit val rw: RW[SessionConfig] = RW.gen
    }
  }

  /**
    * If set to true the server will automatically restart when any configuration option changes. If set to false
    * the server must be manually restarted before changes will take effect.
    *
    * Default is true.
    */
  lazy val autoRestart: Var[Boolean] = Var(if (Profig("autoRestart").exists()) Profig("autoRestart").as[Boolean] else true)

  /**
    * Listeners for the server. Support HTTP and HTTPS listeners. Use addHttpListener and addHttpsListener for easier
    * usage.
    *
    * Defaults to one HTTP listener on 127.0.0.1:8080. This can be managed in code or via configuration. Storing an
    * HttpServerListener instance to "listeners.http" or HttpsServerListener to "listeners.https" will override the
    * defaults if done before this property is accessed for the first time. This can also be overridden via command-line
    * using specifics like "-listeners.http.host=0.0.0.0". HTTPS is configured by default, but enabled is set to false.
    * To easily enable HTTPS just pass "-listeners.https.enabled=true".
    */
  lazy val listeners: Var[List[ServerSocketListener]] = prop(List(
    Profig("listeners.http").asOr[HttpServerListener](HttpServerListener()),
    Profig("listeners.https").asOr[HttpsServerListener](HttpsServerListener())
  ))

  def enabledListeners: List[ServerSocketListener] = listeners().filter(_.enabled)

  def clearListeners(): ServerConfig = {
    listeners @= Nil
    this
  }

  def addListeners(listeners: ServerSocketListener*): ServerConfig = {
    this.listeners @= this.listeners() ::: listeners.toList
    this
  }

  protected def prop[T](value: T): Var[T] = {
    val v = Var[T](value)
    v.attach { value =>
      if (autoRestart.get && server.isRunning) {
        server.restart()
      }
    }
    v
  }
}