package spice.http.server

import rapid.Task
import spice.http.{CacheControl, HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.dsl.string2Content
import spice.net.ContentType

object DefaultErrorHandler extends ErrorHandler {
  lazy val lastModified: Long = System.currentTimeMillis()

  def html(status: HttpStatus): Content = s"""<html>
    <head>
      <title>Error ${status.code}</title>
    </head>
    <body>
      ${status.code} - ${status.message}
    </body>
  </html>""".withContentType(ContentType.`text/html`).withLastModified(lastModified)

  override def handle(exchange: HttpExchange, t: Option[Throwable]): Task[HttpExchange] = {
    exchange.modify { response => Task {
      val status = if (response.status.isError) {
        response.status
      } else {
        HttpStatus.InternalServerError
      }
      response
        .withContent(html(status))
        .withHeader(CacheControl(CacheControl.NoCache))
    }}
  }
}