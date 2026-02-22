package spice.http.server

import fabric.*
import rapid.Task
import spice.http.{CacheControl, Headers, HttpExchange, HttpStatus}
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

  def json(status: HttpStatus, throwable: Option[Throwable] = None): Content = {
    val errorObj = obj(
      "status" -> num(status.code),
      "error" -> str(status.message),
      "message" -> throwable.map(t => str(t.getMessage)).getOrElse(str(status.message))
    )
    Content.json(errorObj)
  }

  private def acceptsJson(exchange: HttpExchange): Boolean = {
    val accept = exchange.request.headers.get(Headers.Request.`Accept`)
    accept.exists(_.contains("application/json"))
  }

  override def handle(exchange: HttpExchange, t: Option[Throwable]): Task[HttpExchange] = {
    exchange.modify { response => Task {
      val status = if (response.status.isError) {
        response.status
      } else {
        HttpStatus.InternalServerError
      }
      val content = if (acceptsJson(exchange)) json(status, t) else html(status)
      response
        .withContent(content)
        .withHeader(CacheControl(CacheControl.NoCache))
    }}
  }
}