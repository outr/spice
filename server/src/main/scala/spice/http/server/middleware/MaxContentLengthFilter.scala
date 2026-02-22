package spice.http.server.middleware

import rapid.Task
import scribe.mdc.MDC
import spice.http.{Headers, HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.dsl.{ConnectionFilter, FilterResponse}

class MaxContentLengthFilter(maxBytes: Long) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = Task {
    val contentLength = Headers.`Content-Length`.value(exchange.request.headers).getOrElse(0L)
    if (contentLength > maxBytes) {
      stop(exchange.copy(response = exchange.response
        .withStatus(HttpStatus.RequestEntityTooLarge)
        .withContent(Content.string(s"Request body too large. Maximum: $maxBytes bytes", spice.net.ContentType.`text/plain`))
      ))
    } else {
      continue(exchange)
    }
  }
}

object MaxContentLengthFilter {
  def apply(maxBytes: Long): MaxContentLengthFilter = new MaxContentLengthFilter(maxBytes)
}
