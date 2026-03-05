package spice.api.server

import fabric.{Json, obj}
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw.*
import rapid.Task
import spice.http.{HttpMethod, HttpStatus}
import spice.http.content.{Content, StringContent}
import spice.http.server.MutableHttpServer
import spice.net.{ContentType, URL, URLMatcher, URLPath}

object ApiServerRuntime {
  private def pathMatcher(basePath: URLPath, methodName: String): URLMatcher = {
    val fullPath = basePath.merge(URLPath.parse("/" + methodName))
    (url: URL) => url.path == fullPath
  }

  def mountGet[R: RW](
    server: MutableHttpServer,
    basePath: URLPath,
    methodName: String,
    handler: () => Task[R]
  ): Unit = {
    server.handler
      .matcher(pathMatcher(basePath, methodName))
      .handle { exchange =>
        handler().flatMap { response =>
          exchange.modify { httpResponse =>
            Task(httpResponse
              .withContent(Content.json(response.json))
              .withStatus(HttpStatus.OK))
          }
        }.handleError { throwable =>
          exchange.modify { httpResponse =>
            Task(httpResponse
              .withContent(errorContent(throwable))
              .withStatus(HttpStatus.InternalServerError))
          }
        }
      }
  }

  def mountRestful[Req: RW, Res: RW](
    server: MutableHttpServer,
    basePath: URLPath,
    methodName: String,
    handler: Req => Task[Res]
  ): Unit = {
    server.handler
      .matcher(pathMatcher(basePath, methodName))
      .handle { exchange =>
        extractJson(exchange).flatMap { json =>
          val request = json.as[Req]
          handler(request).flatMap { response =>
            exchange.modify { httpResponse =>
              Task(httpResponse
                .withContent(Content.json(response.json))
                .withStatus(HttpStatus.OK))
            }
          }
        }.handleError { throwable =>
          exchange.modify { httpResponse =>
            Task(httpResponse
              .withContent(errorContent(throwable))
              .withStatus(HttpStatus.InternalServerError))
          }
        }
      }
  }

  def mountJson[R: RW](
    server: MutableHttpServer,
    basePath: URLPath,
    methodName: String,
    handler: Json => Task[R]
  ): Unit = {
    server.handler
      .matcher(pathMatcher(basePath, methodName))
      .handle { exchange =>
        extractJson(exchange).flatMap { json =>
          handler(json).flatMap { response =>
            exchange.modify { httpResponse =>
              Task(httpResponse
                .withContent(Content.json(response.json))
                .withStatus(HttpStatus.OK))
            }
          }
        }.handleError { throwable =>
          exchange.modify { httpResponse =>
            Task(httpResponse
              .withContent(errorContent(throwable))
              .withStatus(HttpStatus.InternalServerError))
          }
        }
      }
  }

  private def extractJson(exchange: spice.http.HttpExchange): Task[Json] = {
    exchange.request.content match {
      case Some(content) => content.asString.map { s =>
        if (s.isEmpty) obj() else JsonParser(s)
      }
      case None => Task.pure(obj())
    }
  }

  private def errorContent(throwable: Throwable): Content = {
    val message = Option(throwable.getMessage).getOrElse("Internal server error")
    Content.json(obj(
      "error" -> obj(
        "message" -> message.json,
        "code" -> 500.json
      )
    ))
  }
}
