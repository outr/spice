package spice.http.server.dsl

import rapid.Task
import scribe.mdc.MDC
import spice.http.HttpExchange
import spice.net.IP

case class IPAddressFilter(allow: List[IP] = Nil, deny: List[IP] = Nil) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(implicit mdc: MDC): Task[FilterResponse] = Task {
    val ip = exchange.request.originalSource
    val allowed = if (allow.isEmpty) true else allow.contains(ip)
    val denied = if (deny.isEmpty) false else deny.contains(ip)
    if (allowed && !denied) {
      FilterResponse.Continue(exchange)
    } else {
      FilterResponse.Stop(exchange)
    }
  }
}