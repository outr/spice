package spice.net

import io.circe.Decoder.Result
import io.circe._

trait IP extends Location {
  val address: Vector[Int]
  val addressString: String

  override def equals(obj: Any): Boolean = obj match {
    case that: IP if this.addressString == that.addressString => true
    case _ => false
  }

  override def toString: String = addressString
}

object IP {
  def LocalHost: IP = IPv4.LocalHost

  implicit val encoder: Encoder[IP] = new Encoder[IP] {
    override def apply(a: IP): Json = Json.arr(a.address.map(Json.fromInt): _*)
  }

  implicit val decoder: Decoder[IP] = new Decoder[IP] {
    override def apply(c: HCursor): Result[IP] = c.value.asArray match {
      case Some(array) => Right(IP(array.map(_.asNumber.flatMap(_.toInt).getOrElse(throw MalformedIPAddressException(c.value.toString())))))
      case None => Left(DecodingFailure(s"Expected array, got: ${c.value}", Nil))
    }
  }

  def get(address: String): Option[IP] = try {
    address match {
      case IPv4(ip) => Some(ip)
      case _ if address.contains(':') => Some(IPv6(address))
      case _ => None
    }
  } catch {
    case _: MalformedIPAddressException => None
  }

  def unapply(address: String): Option[IP] = get(address)

  def apply(address: String): IP = get(address).getOrElse(throw MalformedIPAddressException(address))
  def apply(a: Vector[Int]): IP = if (a.length == 4) {
    IPv4(a(0), a(1), a(2), a(3))
  } else if (a.length == 8) {
    IPv6(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7))
  } else {
    throw MalformedIPAddressException(a.toString())
  }
}