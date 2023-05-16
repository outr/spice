package spice.http.server.handler

import cats.effect.IO
import scribe.Priority
import scribe.data.MDC
import spice.http.content.Content
import spice.http.{HttpExchange, HttpStatus, StringHeaderKey}

trait HttpHandler extends Ordered[HttpHandler] {
  def priority: Priority = Priority.Normal

  def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange]

  override def compare(that: HttpHandler): Int = Priority.PriorityOrdering.compare(this.priority, that.priority)
}

object HttpHandler {
  def redirect(exchange: HttpExchange, location: String): IO[HttpExchange] = {
    val isStreaming = exchange.request.headers.first(new StringHeaderKey("streaming")).contains("true")
    exchange.modify { response => IO {
      if (isStreaming) {
        val status = HttpStatus.NetworkAuthenticationRequired(s"Redirect to $location")
        response
          .withStatus(status)
          .withContent(Content.empty)
      } else {
        response.withRedirect(location)
      }
    }}.map(_.finish())
  }
}