package spice.http.server.middleware

import rapid.Task
import scribe.mdc.MDC
import spice.http.{Headers, HttpExchange}
import spice.http.server.dsl.{ConnectionFilter, FilterResponse}

class SecurityHeadersFilter(
  hstsMaxAge: Option[Long] = Some(31536000L),
  hstsIncludeSubDomains: Boolean = true,
  frameOptions: Option[String] = Some("DENY"),
  contentTypeOptions: Boolean = true,
  contentSecurityPolicy: Option[String] = None
) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
    exchange.modify { response =>
      Task.pure {
        var r = response
        hstsMaxAge.foreach { maxAge =>
          val value = if (hstsIncludeSubDomains) {
            s"max-age=$maxAge; includeSubDomains"
          } else {
            s"max-age=$maxAge"
          }
          r = r.withHeader(Headers.Response.`Strict-Transport-Security`(value))
        }
        frameOptions.foreach { fo =>
          r = r.withHeader(Headers.Response.`X-Frame-Options`(fo))
        }
        if (contentTypeOptions) {
          r = r.withHeader(Headers.Response.`X-Content-Type-Options`("nosniff"))
        }
        contentSecurityPolicy.foreach { csp =>
          r = r.withHeader(Headers.Response.`Content-Security-Policy`(csp))
        }
        r
      }
    }.map(continue)
  }
}

object SecurityHeadersFilter {
  def apply(
    hstsMaxAge: Option[Long] = Some(31536000L),
    hstsIncludeSubDomains: Boolean = true,
    frameOptions: Option[String] = Some("DENY"),
    contentTypeOptions: Boolean = true,
    contentSecurityPolicy: Option[String] = None
  ): SecurityHeadersFilter = new SecurityHeadersFilter(
    hstsMaxAge, hstsIncludeSubDomains, frameOptions, contentTypeOptions, contentSecurityPolicy
  )

  val Default: SecurityHeadersFilter = apply()
}
