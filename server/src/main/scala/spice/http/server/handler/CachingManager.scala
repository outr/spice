package spice.http.server.handler

import cats.effect.IO
import scribe.data.MDC
import spice.http.{CacheControl, Headers, HttpExchange, HttpStatus}

sealed trait CachingManager extends HttpHandler

object CachingManager {
  case object Default extends CachingManager {
    override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = IO.pure(exchange)
  }
  case object NotCached extends CachingManager {
    override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = exchange.modify { response =>
      IO(response.withHeader(Headers.`Cache-Control`(CacheControl.NoCache, CacheControl.NoStore)))
    }
  }
  case class LastModified(publicCache: Boolean = true) extends CachingManager {
    val visibility: CacheControl = if (publicCache) CacheControl.Public else CacheControl.Private

    override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = {
      exchange.modify { response =>
        response.content match {
          case Some(content) => IO {
            val ifModifiedSince = Headers.Request.`If-Modified-Since`.value(exchange.request.headers).getOrElse(0L)
            if (ifModifiedSince == content.lastModified) {
              response.copy(status = HttpStatus.NotModified, headers = Headers.empty, content = None)
            } else {
              response
                .withHeader(Headers.`Cache-Control`(visibility))
                .withHeader(Headers.Response.`Last-Modified`(content.lastModified))
            }
          }
          case None => IO.pure(response)
        }
      }
    }
  }
  case class MaxAge(seconds: Long) extends CachingManager {
    override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = exchange.modify { response =>
      IO(response.withHeader(Headers.`Cache-Control`(CacheControl.MaxAge(seconds))))
    }
  }
}