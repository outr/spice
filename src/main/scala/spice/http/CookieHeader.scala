package spice.http

import spice.http.cookie._

object CookieHeader extends ListTypedHeaderKey[Cookie.Request] {
  override def key: String = "Cookie"
  override protected def commaSeparated: Boolean = false

  override def value(headers: Headers): List[Cookie.Request] = {
    val cookies = headers.get(this)
    try {
      cookies.flatMap(_.split(';')).map(_.trim).collect {
        case SetCookie.KeyValueRegex(key, value) => Cookie.Request(key, value)
      }
    } catch {
      case t: Throwable =>
        scribe.error(new RuntimeException(s"Failed to parse cookie: [${cookies.mkString("|")}]", t))
        throw t
    }
  }

  override def apply(value: Cookie.Request): Header = Header(this, value.http)
}