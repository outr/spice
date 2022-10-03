package spice.http.server.validation

import cats.effect.IO
import spice.http.{HttpExchange, HttpStatus}
import spice.net.IP

class IPAddressValidator(allow: Set[IP], reject: Set[IP], defaultAllow: Boolean) extends Validator {
  override def validate(exchange: HttpExchange): IO[ValidationResult] = IO {
    val ip = exchange.request.originalSource
    if ((allow.contains(ip) || defaultAllow) && !reject.contains(ip)) {
      ValidationResult.Continue(exchange)
    } else {
      scribe.warn(s"Unauthorized attempt to access: ${exchange.request.url} from IP: $ip. Allowed: ${allow.mkString(", ")}, Reject: ${reject.mkString(", ")}, Default Allow? $defaultAllow")
      ValidationResult.Error(exchange, HttpStatus.Forbidden.code, s"Unauthorized IP address: $ip")
    }
  }
}