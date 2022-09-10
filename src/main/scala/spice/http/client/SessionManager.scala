package spice.http.client

import spice.http.cookie.Cookie

class SessionManager(private var _session: Session = Session()) {
  def session: Session = _session

  def apply(cookies: List[Cookie.Response]): Session = synchronized {
    val filtered = session.cookies.filterNot(cookies.contains)
    _session = session.copy(filtered ::: cookies)
    session
  }
}