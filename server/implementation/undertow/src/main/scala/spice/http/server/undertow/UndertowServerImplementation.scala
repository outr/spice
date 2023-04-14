package spice.http.server.undertow

import cats.effect.IO
import io.undertow.{Undertow, UndertowOptions}
import io.undertow.predicate.Predicates
import io.undertow.server.{HttpServerExchange, HttpHandler => UndertowHttpHandler}
import io.undertow.server.handlers.encoding.{ContentEncodingRepository, DeflateEncodingProvider, EncodingHandler, GzipEncodingProvider}
import moduload.Moduload
import reactify._
import scribe.Logger
import spice.http.server.config.{HttpServerListener, HttpsServerListener}
import spice.http.{HttpExchange, HttpResponse}
import spice.http.server.{HttpServer, HttpServerImplementation, HttpServerImplementationManager, SSLUtil, ServerStartException}
import spice.net.{MalformedURLException, URL}

import java.util.logging.LogManager
import cats.effect.unsafe.implicits.global

import scala.jdk.CollectionConverters.ListHasAsScala

class UndertowServerImplementation(server: HttpServer) extends HttpServerImplementation with UndertowHttpHandler {
  private val instance: Var[Option[Undertow]] = Var(None)

  override def start(server: HttpServer): IO[Unit] = IO {
    val contentEncodingRepository = new ContentEncodingRepository()
      .addEncodingHandler("gzip", new GzipEncodingProvider, 100, Predicates.requestLargerThan(5L))
      .addEncodingHandler("deflate", new DeflateEncodingProvider, 50, Predicates.requestLargerThan(5L))
    val encodingHandler = new EncodingHandler(contentEncodingRepository).setNext(this)

    val builder = Undertow.builder().setHandler(encodingHandler)
    if (server.config.enableHTTP2) {
      builder.setServerOption(UndertowOptions.ENABLE_HTTP2, java.lang.Boolean.TRUE)
    }

    server.config.listeners.foreach {
      case HttpServerListener(host, port, enabled, _, _) => if (enabled) {
        builder.addHttpListener(port.getOrElse(0), host)
      }
      case HttpsServerListener(host, port, keyStore, enabled, _, _) => if (enabled) {
        try {
          val sslContext = SSLUtil.createSSLContext(keyStore.location, keyStore.password)
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
        val address = s"${info.getProtcol}:/${info.getAddress}"
        URL.parse(address)
      }
      server.config.listeners @= server.config.listeners().zip(urls).map {
        case (listener, url) =>
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
      }
    } catch {
      case t: Throwable => throw ServerStartException(t.getMessage, server.config.listeners(), t)
    }
    instance @= Some(u)
  }

  override def isRunning: Boolean = instance().nonEmpty

  override def stop(server: HttpServer): IO[Unit] = IO {
    instance() match {
      case Some(u) =>
        u.stop()
        instance @= None
      case None => // Not running
    }
  }

  override def handleRequest(undertow: HttpServerExchange): Unit = {
    try {
      val url = URL.parse(s"${undertow.getRequestURL}?${undertow.getQueryString}")
      if (!server.config.persistentConnections()) {
        undertow.setPersistent(false)
      }

      undertow.dispatch(new Runnable {
        override def run(): Unit = {
          val io = UndertowRequestParser(undertow, url).flatMap { request =>
            val exchange = HttpExchange(request, HttpResponse())
            server.handle(exchange)
              .redeemWith(server.errorRecovery(exchange, _), IO.pure)
          }.flatMap { exchange =>
            exchange.webSocketListener match {
              case Some(webSocketListener) => UndertowWebSocketHandler(undertow, server, exchange, webSocketListener)
              case None => UndertowResponseSender(undertow, server, exchange)
            }
          }.redeemWith({ throwable =>
            scribe.error("Unrecoverable error parsing request!", throwable)
            throw throwable
          }, IO.pure)
          io.unsafeRunAndForget()(server.ioRuntime)
        }
      })
    } catch {
      case exc: MalformedURLException => scribe.warn(exc.message)
      case throwable: Throwable => server.errorLogger(throwable, None, None).unsafeRunAndForget()
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