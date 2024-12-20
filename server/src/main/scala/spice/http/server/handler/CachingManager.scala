package spice.http.server.handler

import rapid._
import scribe.mdc.MDC
import spice.http.{CacheControl, Headers, HttpExchange, HttpStatus}

sealed trait CachingManager extends HttpHandler

object CachingManager {
  case object Default extends CachingManager {
    override def handle(exchange: HttpExchange)(implicit mdc: MDC): Task[HttpExchange] = Task.pure(exchange)
  }
  case object NotCached extends CachingManager {
    override def handle(exchange: HttpExchange)(implicit mdc: MDC): Task[HttpExchange] = exchange.modify { response =>
      Task(response.withHeader(Headers.`Cache-Control`(CacheControl.NoCache, CacheControl.NoStore)))
    }
  }
  case class LastModified(publicCache: Boolean = true) extends CachingManager {
    val visibility: CacheControl = if (publicCache) CacheControl.Public else CacheControl.Private

    override def handle(exchange: HttpExchange)(implicit mdc: MDC): Task[HttpExchange] = {
      exchange.modify { response =>
        response.content match {
          case Some(content) => Task {
            val ifModifiedSince = Headers.Request.`If-Modified-Since`.value(exchange.request.headers).getOrElse(0L)
            if (ifModifiedSince == content.lastModified) {
              response.copy(status = HttpStatus.NotModified, headers = Headers.empty, content = None)
            } else {
              response
                .withHeader(Headers.`Cache-Control`(visibility))
                .withHeader(Headers.Response.`Last-Modified`(content.lastModified))
            }
          }
          case None => Task.pure(response)
        }
      }
    }
  }
  case class MaxAge(seconds: Long) extends CachingManager {
    override def handle(exchange: HttpExchange)(implicit mdc: MDC): Task[HttpExchange] = exchange.modify { response =>
      Task(response.withHeader(Headers.`Cache-Control`(CacheControl.MaxAge(seconds))))
    }
  }
}