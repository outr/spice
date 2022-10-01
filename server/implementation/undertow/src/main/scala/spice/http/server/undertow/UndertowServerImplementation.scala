package spice.http.server.undertow

import cats.effect.IO
import io.undertow.{Undertow, UndertowOptions}
import io.undertow.predicate.Predicates
import io.undertow.server.{HttpServerExchange, HttpHandler => UndertowHttpHandler}
import io.undertow.server.handlers.encoding.{ContentEncodingRepository, DeflateEncodingProvider, EncodingHandler, GzipEncodingProvider}
import moduload.Moduload
import reactify._
import scribe.Logger
import spice.http.{HttpConnection, HttpResponse}
import spice.http.server.{HttpServer, HttpServerImplementation, HttpServerImplementationManager, HttpServerListener, HttpsServerListener, SSLUtil}
import spice.net.{MalformedURLException, URL}

import java.util.logging.LogManager

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
      case HttpServerListener(host, port, enabled) => if (enabled) {
        builder.addHttpListener(port, host)
      }
      case HttpsServerListener(host, port, keyStore, enabled) => if (enabled) {
        try {
          val sslContext = SSLUtil.createSSLContext(keyStore.location, keyStore.password)
          builder.addHttpsListener(port, host, sslContext)
        } catch {
          case t: Throwable => {
            throw new RuntimeException(s"Error loading HTTPS, host: $host, port: $port, keyStore: ${keyStore.path}", t)
          }
        }
      }
      case listener => throw new UnsupportedOperationException(s"Unsupported listener: $listener")
    }
    val u = builder.build()
    u.start()
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

  override def handleRequest(exchange: HttpServerExchange): Unit = server.errorSupport {
    try {
      val url = URL(s"${exchange.getRequestURL}?${exchange.getQueryString}")
      if (!server.config.persistentConnections()) {
        exchange.setPersistent(false)
      }

      exchange.dispatch(new Runnable {
        override def run(): Unit = {
          val io = UndertowRequestParser(exchange, url).flatMap { request =>
            val connection = HttpConnection(request, HttpResponse())
            server.handle(connection)
              .redeemWith(server.errorRecovery(connection, _), IO.pure)
          }.flatMap { connection =>
            UndertowResponseSender(exchange, server, connection)
          }
          io.unsafeRunAndForget()(server.ioRuntime)
        }
      })
    } catch {
      case exc: MalformedURLException => scribe.warn(exc.message)
    }
  }
}

object UndertowServerImplementation extends Moduload {
  override def load(): Unit = {
    LogManager.getLogManager.reset()
    Logger.system.installJUL()

    scribe.info(s"Registering UndertowServerImplementation...")
    HttpServerImplementationManager.register(new UndertowServerImplementation(_))
  }

  override def error(t: Throwable): Unit = {
    scribe.error("Error while attempting to register UndertowServerImplementation", t)
  }
}