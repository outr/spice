package spice.net

import scala.util.matching.Regex

case class IPv4(a: Int = 127, b: Int = 0, c: Int = 0, d: Int = 1) extends IP {
  override val address: Vector[Int] = Vector(a, b, c, d)
  override val addressString: String = s"$a.$b.$c.$d"
}

object IPv4 {
  lazy val LocalHost: IPv4 = IPv4()

  private val Regex: Regex = """\b(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b""".r

  def apply(address: String): IPv4 = address match {
    case IPv4(ip) => ip
    case _ => throw MalformedIPAddressException(address)
  }

  def unapply(address: String): Option[IPv4] = address match {
    case Regex(a, b, c, d) => Some(apply(a.toInt, b.toInt, c.toInt, d.toInt))
    case _ => None
  }
}