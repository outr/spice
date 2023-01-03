package spice.http.client

sealed trait ProxyType

object ProxyType {
  case object Direct extends ProxyType

  case object Http extends ProxyType

  case object Socks extends ProxyType
}