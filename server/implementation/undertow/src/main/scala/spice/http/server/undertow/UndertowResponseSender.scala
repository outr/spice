package spice.http.server.undertow

import cats.effect.IO
import io.undertow.io.{IoCallback, Sender}
import io.undertow.server.HttpServerExchange
import io.undertow.server.handlers.resource.URLResource
import io.undertow.util.HttpString
import spice.http.content.{BytesContent, FileContent, StringContent, URLContent}
import spice.http.server.HttpServer
import spice.http.{Headers, HttpConnection, HttpResponse, StreamContent}

import java.io.IOException
import java.nio.ByteBuffer
import scala.jdk.CollectionConverters.SeqHasAsJava

object UndertowResponseSender {
  def apply(exchange: HttpServerExchange,
            server: HttpServer,
            connection: HttpConnection): IO[Unit] = {
    finalResponse(server, connection).flatMap { response =>
      IO {
        exchange.setStatusCode(response.status.code)
        response.headers.map.foreach {
          case (key, values) => exchange.getResponseHeaders.putAll(new HttpString(key), values.asJava)
        }

        if (exchange.getRequestMethod.toString != "HEAD") {
          response.content match {
            case Some(content) => content match {
              case StringContent(s, _, _) => {
                exchange.getResponseSender.send(s, new IoCallback {
                  override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
                    if (s.nonEmpty) {
                      exchange.endExchange()
                    }
                    sender.close()
                  }

                  override def onException(exchange: HttpServerExchange,
                                           sender: Sender,
                                           exception: IOException): Unit = {
                    sender.close()
                    if (exception.getMessage == "Stream closed") {
                      scribe.warn(s"Stream closed for $s")
                    } else {
                      server.error(exception)
                    }
                  }
                })
              }
              case fc: FileContent => ResourceServer.serve(exchange, fc)
              case URLContent(url, _, _) => {
                val resource = new URLResource(url, "")
                resource.serve(exchange.getResponseSender, exchange, new IoCallback {
                  override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
                    sender.close()
                  }

                  override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit = {
                    sender.close()
                    if (exception.getMessage == "Stream closed") {
                      scribe.warn(s"Stream closed for $url")
                    } else {
                      server.error(exception)
                    }
                  }
                })
              }
              case c: BytesContent => {
                val buffer = ByteBuffer.wrap(c.value)
                exchange.getResponseSender.send(buffer, new IoCallback {
                  override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
                    sender.close()
                  }

                  override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit = {
                    sender.close()
                    if (exception.getMessage == "Stream closed") {
                      scribe.warn("Stream closed for BytesContent")
                    } else {
                      server.error(exception)
                    }
                  }
                })
              }
              case c: StreamContent => {
                val runnable = new Runnable {
                  override def run(): Unit = try {
                    exchange.startBlocking()
                    val out = exchange.getOutputStream
                    c.stream(out)
                  } catch {
                    case exc: IOException if exc.getMessage == "Stream closed" => scribe.warn("Stream closed for StreamContent")
                    case t: Throwable => throw t
                  }
                }
                if (exchange.isInIoThread) { // Must move to async thread before blocking
                  exchange.dispatch(runnable)
                } else {
                  runnable.run()
                }
              }
            }
            case None => exchange.getResponseSender.send("", new IoCallback {
              override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
                sender.close()
              }

              override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit = {
                sender.close()
                server.error(exception)
              }
            })
          }
        }
      }
    }
  }

  private def finalResponse(server: HttpServer, connection: HttpConnection): IO[HttpResponse] = IO {
    var response = connection.response

    // Add the Server header if not already set
    if (Headers.Response.`Server`.value(response.headers).isEmpty) {
      response = response.withHeader(Headers.Response.`Server`(server.config.name()))
    }

    connection.response.content.map { content =>
      // Add Content-Type from Content if not already set on the response
      if (Headers.`Content-Type`.value(response.headers).isEmpty) {
        response = response.withHeader(Headers.`Content-Type`(content.contentType))
      }

      // Set the Content-Length from Content if not already set on the response
      if (Headers.`Content-Length`.value(response.headers).isEmpty && content.length != -1L) {
        response = response.withHeader(Headers.`Content-Length`(content.length))
      }

      response
    }.getOrElse(connection.response)
  }
}
