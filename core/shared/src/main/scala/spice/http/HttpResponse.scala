package spice.http

import spice.http.content.Content
import spice.http.cookie.Cookie
import spice.http.cookie.Cookie.Response

case class HttpResponse(status: HttpStatus = HttpStatus.OK,
                        headers: Headers = Headers.empty,
                        content: Option[Content] = None) {
  lazy val cookies: List[Cookie.Response] = Headers.Response.`Set-Cookie`.value(headers)

  def withStatus(status: HttpStatus): HttpResponse = copy(status = status)
  def withHeader(header: Header): HttpResponse = copy(headers = headers.withHeader(header))
  def withHeader(key: String, value: String): HttpResponse = copy(headers = headers.withHeader(key, value))
  def setHeader(header: Header): HttpResponse = copy(headers = headers.setHeader(header))
  def removeHeader(header: HeaderKey): HttpResponse = copy(headers = headers.removeHeader(header))
  def withContent(content: Content, updateHeaders: Boolean = true): HttpResponse = {
    val modified = copy(content = Some(content))
    if (updateHeaders && content != Content.none) {
      modified
        .setHeader(Headers.`Content-Type`(content.contentType))
        .setHeader(Headers.`Content-Length`(content.length))
    } else {
      modified
    }
  }

  def removeContent(): HttpResponse =
    copy(content = None)
      .removeHeader(Headers.`Content-Type`)
      .removeHeader(Headers.`Content-Length`)
  def withRedirect(uri: String, status: HttpStatus = HttpStatus.Found): HttpResponse = copy(status = status).setHeader(Headers.Response.`Location`(uri))
}