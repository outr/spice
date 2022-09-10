package spice.http.client

import spice.http.cookie.Cookie

case class Session(cookies: List[Cookie.Response] = Nil)