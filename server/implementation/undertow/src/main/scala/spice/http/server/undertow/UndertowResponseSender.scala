package spice.http.server.undertow

import cats.effect.IO
import io.undertow.io.{IoCallback, Sender}
import io.undertow.server.HttpServerExchange
import io.undertow.server.handlers.resource.URLResource
import io.undertow.util.HttpString
import spice.http.content.{BytesContent, Content, FileContent, IOStreamContent, StreamContent, URLContent}
import spice.http.server.HttpServer
import spice.http.{Headers, HttpExchange, HttpResponse}

import java.io.IOException
import java.nio.ByteBuffer
import scala.jdk.CollectionConverters.SeqHasAsJava
import scribe.cats.{io => logger}
import cats.effect.unsafe.implicits.global

object UndertowResponseSender {
  def apply(undertow: HttpServerExchange,
            server: HttpServer,
            exchange: HttpExchange): IO[Unit] = {
    finalResponse(server, exchange).flatMap { response =>
      IO[Unit] {
        undertow.setStatusCode(response.status.code)
        response.headers.map.foreach {
          case (key, values) => undertow.getResponseHeaders.putAll(new HttpString(key), values.asJava)
        }

        if (undertow.getRequestMethod.toString != "HEAD") {
          response.content match {
            case Some(content) if content != Content.none => content match {
              case fc: FileContent => ResourceServer.serve(undertow, fc)
              case URLContent(url, _, _) =>
                val resource = new URLResource(url, "")
                resource.serve(undertow.getResponseSender, undertow, new IoCallback {
                  override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
                    sender.close()
                  }

                  override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit = {
                    sender.close()
                    if (exception.getMessage == "Stream closed") {
                      scribe.warn(s"Stream closed for $url")
                    } else {
                      server.errorLogger(exception, None, None).unsafeRunAndForget()
                    }
                  }
                })
              case c: BytesContent =>
                val buffer = ByteBuffer.wrap(c.value)
                undertow.getResponseSender.send(buffer, new IoCallback {
                  override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
                    sender.close()
                  }

                  override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit = {
                    sender.close()
                    if (exception.getMessage == "Stream closed") {
                      scribe.warn("Stream closed for BytesContent")
                    } else {
                      server.errorLogger(exception, None, None).unsafeRunAndForget()
                    }
                  }
                })
              case c: IOStreamContent =>
                undertow.startBlocking()
                val out = undertow.getOutputStream
                c.stream(out)
              case StreamContent(stream, _, _, _) =>
                undertow.startBlocking()
                val out = undertow.getOutputStream
                stream
                  .chunkN(1024, allowFewer = true)
                  .map(chunk => out.write(chunk.toArray))
                  .compile
                  .drain
                  .map { _ =>
                    out.flush()
                    out.close()
                    undertow.endExchange()
                  }
                  .unsafeRunAndForget()(server.ioRuntime)
              case _ =>
                val contentString = content.asString
                undertow.getResponseSender.send(contentString, new IoCallback {
                  override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
                    if (contentString.nonEmpty) {
                      exchange.endExchange()
                    }
                    sender.close()
                  }

                  override def onException(exchange: HttpServerExchange,
                                           sender: Sender,
                                           exception: IOException): Unit = {
                    sender.close()
                    if (exception.getMessage == "Stream closed") {
                      scribe.warn(s"Stream closed for $contentString")
                    } else {
                      server.errorLogger(exception, None, None).unsafeRunAndForget()
                    }
                  }
                })
            }
            case _ =>
              undertow.getResponseSender.send("", new IoCallback {
                override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
                  sender.close()
                }

                override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit = {
                  sender.close()
                  server.errorLogger(exception, None, None).unsafeRunAndForget()
                }
              })
          }
        }
      }.handleErrorWith { throwable =>
        logger.error(s"Error occurred sending response: $response", throwable)
      }
    }
  }

  private def finalResponse(server: HttpServer, exchange: HttpExchange): IO[HttpResponse] = IO {
    var response = exchange.response

    // Add the Server header if not already set
    if (Headers.Response.`Server`.value(response.headers).isEmpty) {
      response = response.withHeader(Headers.Response.`Server`(server.config.name()))
    }

    exchange.response
  }
}
