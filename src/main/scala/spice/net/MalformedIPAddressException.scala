package spice.net

case class MalformedIPAddressException(value: String) extends RuntimeException(s"Malformed IP address: $value")
