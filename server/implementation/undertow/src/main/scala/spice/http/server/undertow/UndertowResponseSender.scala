package spice.http.server.undertow

import rapid.*
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

object UndertowResponseSender {
  def apply(undertow: HttpServerExchange,
            server: HttpServer,
            exchange: HttpExchange): Task[Unit] = {
    finalResponse(server, exchange).flatMap { response =>
      Task[Option[Content]] {
        undertow.setStatusCode(response.status.code)
        response.headers.map.foreach {
          case (key, values) => undertow.getResponseHeaders.putAll(new HttpString(key), values.asJava)
        }
        response.content.filterNot(c => undertow.getRequestMethod.toString == "HEAD" && c != Content.none)
      }.flatMap(o => sendContent(o, undertow, server)).handleError { throwable =>
        logger.error(s"Error occurred sending response: $response", throwable)
      }
    }
  }

  private def sendContent(contentOption: Option[Content], undertow: HttpServerExchange, server: HttpServer): Task[Unit] = {
    contentOption match {
      case Some(content) => content match {
        case fc: FileContent => Task(ResourceServer.serve(undertow, fc))
        case URLContent(url, _, _) => Task {
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
                server.errorLogger(exception, None, None).sync()
              }
            }
          })
        }
        case c: BytesContent => Task {
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
                server.errorLogger(exception, None, None).start()
              }
            }
          })
        }
        case c: IOStreamContent => Task {
          undertow.startBlocking()
          val out = undertow.getOutputStream
          c.stream(out)
        }
        case StreamContent(stream, _, _, _) =>
          undertow.startBlocking()
          val out = undertow.getOutputStream
          stream
            .chunk(1024)
            .map(chunk => out.write(chunk.toArray))
            .drain
            .map { _ =>
              out.flush()
              out.close()
              undertow.endExchange()
            }
        case _ =>
          content.asString.map { contentString =>
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
                  server.errorLogger(exception, None, None).start()
                }
              }
            })
          }
      }
      case None => Task {
        undertow.getResponseSender.send("", new IoCallback {
          override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = {
            sender.close()
          }

          override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit = {
            sender.close()
            server.errorLogger(exception, None, None).start()
          }
        })
      }
    }
  }

  private def finalResponse(server: HttpServer, exchange: HttpExchange): Task[HttpResponse] = Task {
    var response = exchange.response

    // Add the Server header if not already set
    if (Headers.Response.`Server`.value(response.headers).isEmpty) {
      response = response.withHeader(Headers.Response.`Server`(server.config.name()))
    }

    response
  }
}
