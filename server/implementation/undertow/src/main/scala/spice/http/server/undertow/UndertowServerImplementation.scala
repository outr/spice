package spice.http.server.undertow

import rapid.*
import io.undertow.{Undertow, UndertowOptions}
import io.undertow.predicate.Predicates
import io.undertow.server.{HttpServerExchange, HttpHandler as UndertowHttpHandler}
import io.undertow.server.handlers.encoding.{ContentEncodingRepository, DeflateEncodingProvider, EncodingHandler, GzipEncodingProvider}
import moduload.Moduload
import reactify.*
import scribe.Logger
import spice.http.server.config.{HttpServerListener, HttpsServerListener}
import spice.http.{HttpExchange, HttpResponse}
import spice.http.server.{HttpServer, HttpServerImplementation, HttpServerImplementationManager, SSLUtil, ServerStartException}
import spice.net.{MalformedURLException, URL}

import java.util.logging.LogManager
import scribe.mdc.MDC

import scala.jdk.CollectionConverters.ListHasAsScala

class UndertowServerImplementation(server: HttpServer) extends HttpServerImplementation with UndertowHttpHandler {
  private val instance: Var[Option[Undertow]] = Var(None)
  // Per-HTTPS-listener cert reloaders, keyed by (host, configured-port). Populated during
  // start(); consulted by reloadCertificates() to swap key material without restarting
  // the listener. Concurrent because reload can race with a graceful stop.
  private val reloaders: java.util.concurrent.ConcurrentHashMap[(String, Int), spice.http.server.ReloadableSSLContext.Reloader] =
    new java.util.concurrent.ConcurrentHashMap()

  override def start(server: HttpServer): Task[Unit] = Task {
    val contentEncodingRepository = new ContentEncodingRepository()
      .addEncodingHandler("gzip", new GzipEncodingProvider, 100, Predicates.requestLargerThan(5L))
      .addEncodingHandler("deflate", new DeflateEncodingProvider, 50, Predicates.requestLargerThan(5L))
    val encodingHandler = new EncodingHandler(contentEncodingRepository).setNext(this)

    val builder = Undertow.builder().setHandler(encodingHandler)
    // Allow uploads up to 20MB (Undertow default is 2MB)
    builder.setServerOption(UndertowOptions.MAX_ENTITY_SIZE, java.lang.Long.valueOf(20L * 1024 * 1024))
    if (server.config.enableHTTP2) {
      builder.setServerOption(UndertowOptions.ENABLE_HTTP2, java.lang.Boolean.TRUE)
    }

    server.config.listeners.foreach {
      case HttpServerListener(host, port, enabled, _, _) => if (enabled) {
        builder.addHttpListener(port.getOrElse(0), host)
      }
      case HttpsServerListener(host, port, keyStore, enabled, _, _) => if (enabled) {
        try {
          // Always go through the reloadable path so reloadCertificates() can swap the
          // cert without rebuilding the listener. The reloader is tracked by (host, port)
          // so a later renewal can find the right one to update.
          val (sslContext, reloader) = SSLUtil.createReloadableSSLContext(keyStore.location, keyStore.password)
          reloaders.put((host, port.getOrElse(0)), reloader)
          builder.addHttpsListener(port.getOrElse(0), host, sslContext)
        } catch {
          case t: Throwable =>
            throw new RuntimeException(s"Error loading HTTPS, host: $host, port: $port, keyStore: ${keyStore.path}", t)
        }
      }
      case listener => throw new UnsupportedOperationException(s"Unsupported listener: $listener")
    }
    val u = builder.build()
    try {
      u.start()
      val urls = u.getListenerInfo.asScala.map { info =>
        val a = info.getAddress.toString.replace("[", "").replace("]", "")
        val address = s"${info.getProtcol}:/$a"
        try {
          Some(URL.parse(address))
        } catch {
          case t: Throwable =>
            scribe.error(s"Failed to parse $address", t)
            None
        }
      }
      server.config.listeners @= server.config.listeners().zip(urls).map {
        case (listener, urlOption) => urlOption match {
          case Some(url) =>
            val host = if (listener.host == "0.0.0.0") {
              url.host
            } else {
              listener.host
            }
            val port = if (listener.port.isEmpty) {
              Some(url.port)
            } else {
              listener.port
            }
            listener match {
              case l: HttpServerListener => l.copy(host = host, port = port)
              case l: HttpsServerListener => l.copy(host = host, port = port)
            }
          case None => listener
        }
      }
    } catch {
      case t: Throwable => throw ServerStartException(t.getMessage, server.config.listeners(), t)
    }
    instance @= Some(u)
  }

  override def isRunning: Boolean = instance().nonEmpty

  override def stop(server: HttpServer): Task[Unit] = Task {
    instance() match {
      case Some(u) =>
        u.stop()
        instance @= None
        reloaders.clear()
      case None => // Not running
    }
  }

  /** Re-read each enabled HTTPS listener's keystore from disk and atomically swap the
    * inner KeyManager — no listener teardown, no dropped connections. Listeners that
    * weren't HTTPS (or that haven't been started yet) are skipped. */
  override def reloadCertificates(server: HttpServer): Task[Unit] = Task {
    server.config.listeners().foreach {
      case https: HttpsServerListener if https.enabled =>
        val key = (https.host, https.port.getOrElse(0))
        Option(reloaders.get(key)) match {
          case Some(reloader) =>
            try reloader.reloadFrom(https.keyStore.location, https.keyStore.password)
            catch {
              case t: Throwable =>
                scribe.error(s"Failed to reload certificate for $key from ${https.keyStore.path}", t)
                throw t
            }
          case None =>
            scribe.warn(s"reloadCertificates: no reloader registered for $key (server not started?)")
        }
      case _ => ()
    }
  }

  override def handleRequest(undertow: HttpServerExchange): Unit = {
    try {
      val arrivalMs = System.currentTimeMillis()
      val url = URL.parse(s"${undertow.getRequestURL}?${undertow.getQueryString}")
      val isWsUpgrade = Option(undertow.getRequestHeaders.getFirst("Upgrade")).exists(_.equalsIgnoreCase("websocket"))
      if (isWsUpgrade) scribe.info(s"WS upgrade request received for ${url.path}")
      if (!server.config.persistentConnections()) {
        undertow.setPersistent(false)
      }

      undertow.dispatch(new Runnable {
        override def run(): Unit = {
          MDC { mdc =>
            given MDC = mdc
            mdc("url") = url
            val dispatchMs = System.currentTimeMillis()
            if (isWsUpgrade) scribe.info(s"WS dispatch delay: ${dispatchMs - arrivalMs}ms")
            val startMs = dispatchMs
            val io = UndertowRequestParser(undertow, url).flatMap { request =>
              val parseMs = System.currentTimeMillis() - startMs
              if (parseMs > 100) scribe.warn(s"Slow request parse: ${parseMs}ms for ${url.path}")
              val exchange = HttpExchange(request)
              server.handle(exchange).handleError { throwable =>
                server.errorRecovery(exchange, throwable)
              }
            }.flatMap { exchange =>
              val handleMs = System.currentTimeMillis() - startMs
              if (handleMs > 100) scribe.warn(s"Slow handler chain: ${handleMs}ms for ${url.path}")
              exchange.webSocketListener match {
                case Some(webSocketListener) => UndertowWebSocketHandler(undertow, server, exchange, webSocketListener)
                case None => UndertowResponseSender(undertow, server, exchange)
              }
            }.handleError { throwable =>
              scribe.error("Unrecoverable error parsing request!", throwable)
              throw throwable
            }
            io.start()
          }
        }
      })
    } catch {
      case exc: MalformedURLException => scribe.warn(exc.message)
      case throwable: Throwable => server.errorLogger(throwable, None, None).start()
    }
  }
}

object UndertowServerImplementation extends Moduload {
  override def load(): Unit = {
    LogManager.getLogManager.reset()
    Logger.system.installJUL()

    HttpServerImplementationManager.register(new UndertowServerImplementation(_))
  }

  override def error(t: Throwable): Unit = {
    scribe.error("Error while attempting to register UndertowServerImplementation", t)
  }
}