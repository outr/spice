package spice.http.server.middleware

import rapid.Task
import scribe.mdc.MDC
import spice.http.{Headers, HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.dsl.{ConnectionFilter, FilterResponse}

import java.security.MessageDigest

class ETagFilter extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
    Task.pure(continue(exchange))
  }

  override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = {
    super.handle(exchange).flatMap { result =>
      result.response.content match {
        case Some(content) =>
          val etag = s""""${content.lastModified.toHexString}-${content.length.toHexString}""""
          val ifNoneMatch = result.request.headers.first(Headers.Request.`If-None-Match`)

          if (ifNoneMatch.contains(etag)) {
            result.modify { response =>
              Task.pure(
                response
                  .withStatus(HttpStatus.NotModified)
                  .withContent(Content.none)
                  .withHeader(Headers.Response.`ETag`(etag))
              )
            }
          } else {
            result.modify { response =>
              Task.pure(response.withHeader(Headers.Response.`ETag`(etag)))
            }
          }
        case None => Task.pure(result)
      }
    }
  }
}

object ETagFilter {
  def apply(): ETagFilter = new ETagFilter()
}
