package spice.http.server

import rapid.Task
import spice.http.{Headers, HttpExchange, HttpMethod, HttpStatus}
import spice.http.content.Content

trait CORSSupport extends HttpServer {
  protected def allowOrigin: String = "*"
  protected def allowMethods: Set[HttpMethod] = Set(HttpMethod.Get, HttpMethod.Post, HttpMethod.Put, HttpMethod.Delete, HttpMethod.Patch, HttpMethod.Options)
  protected def allowHeaders: Set[String] = Set("Content-Type", "Authorization", "Accept", "Origin", "X-Requested-With")
  protected def exposeHeaders: Set[String] = Set.empty
  protected def maxAge: Option[Long] = Some(86400L)
  protected def allowCredentials: Boolean = false

  override protected def preHandle(exchange: HttpExchange): Task[HttpExchange] = super
    .preHandle(exchange)
    .flatMap { exchange =>
      val isPreflight = exchange.request.method == HttpMethod.Options &&
        exchange.request.headers.contains(Headers.Request.`Origin`)

      exchange.modify { response =>
        Task.pure {
          // setHeader (replace) for every CORS header — these are single-value per RFC 6454
          // and must not be duplicated. `withHeader` would APPEND if any were already set
          // upstream, producing malformed responses (browsers reject duplicate ACAO etc.).
          var r = response
            .setHeader(Headers.Response.`Access-Control-Allow-Origin`(allowOrigin))
            .setHeader(Headers.Response.`Access-Control-Allow-Methods`(allowMethods.map(_.value).mkString(", ")))
            .setHeader(Headers.Response.`Access-Control-Allow-Headers`(allowHeaders.mkString(", ")))

          if (allowCredentials) {
            r = r.setHeader(Headers.Response.`Access-Control-Allow-Credentials`(true))
          }
          maxAge.foreach { age =>
            r = r.setHeader(Headers.Response.`Access-Control-Max-Age`(age.toString))
          }
          if (exposeHeaders.nonEmpty) {
            r = r.setHeader(Headers.Response.`Access-Control-Expose-Headers`(exposeHeaders.mkString(", ")))
          }

          if (isPreflight) {
            r.withStatus(HttpStatus.NoContent).withContent(Content.none)
          } else {
            r
          }
        }
      }
    }
}
