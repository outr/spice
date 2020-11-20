package spice

import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

import scala.util.matching.Regex

// TODO: Add IP address interpolation
object IP {
  def LocalHost: IP = v4.LocalHost

  implicit val encoder: Encoder[IP] = new Encoder[IP] {
    override def apply(a: IP): Json = Json.arr(a.address.map(Json.fromInt): _*)
  }

  implicit val decoder: Decoder[IP] = new Decoder[IP] {
    override def apply(c: HCursor): Result[IP] = c.value.asArray match {
      case Some(array) => Right(IP(array.map(_.asNumber.flatMap(_.toInt).getOrElse(throw MalformedIPAddressException(c.value.toString())))))
      case None => Left(DecodingFailure(s"Expected array, got: ${c.value}", Nil))
    }
  }

  case class v4(a: Int = 127, b: Int = 0, c: Int = 0, d: Int = 1) extends IP {
    override val address: Array[Int] = Array(a, b, c, d)
    override val addressString: String = s"$a.$b.$c.$d"
  }

  object v4 {
    lazy val LocalHost: v4 = v4()

    private val Regex: Regex = """\b(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b""".r

    def apply(address: String): v4 = address match {
      case v4(ip) => ip
      case _ => throw MalformedIPAddressException(address)
    }

    def unapply(address: String): Option[v4] = address match {
      case Regex(a, b, c, d) => Some(apply(a.toInt, b.toInt, c.toInt, d.toInt))
      case _ => None
    }
  }

  case class v6(a: Int = 0,
                b: Int = 0,
                c: Int = 0,
                d: Int = 0,
                e: Int = 0,
                f: Int = 0,
                g: Int = 0,
                h: Int = 1,
                scope: Option[String] = None) extends IP {
    private def nc(i: Int): String = f"$i%04x"

    private def cs(i: Int): String = i.toHexString

    override val address: Array[Int] = Array(a, b, c, d, e, f, g, h)
    override val addressString: String = s"${nc(a)}:${nc(b)}:${nc(c)}:${nc(d)}:${nc(e)}:${nc(f)}:${nc(g)}:${nc(h)}${scope.getOrElse("")}"

    lazy val canonicalString: String = s"${cs(a)}:${cs(b)}:${cs(c)}:${cs(d)}:${cs(e)}:${cs(f)}:${cs(g)}:${cs(h)}${scope.getOrElse("")}"
  }

  object v6 {
    lazy val LocalHost: v6 = v6()

    def apply(address: String): v6 = {
      val percent = address.indexOf('%')
      val (a, scope) = if (percent != -1) {
        (address.substring(0, percent), Some(address.substring(percent + 1)))
      } else {
        (address, None)
      }
      val separator = a.indexOf("::")
      val v: Vector[Int] = (if (separator != -1) {
        val left = a.substring(0, separator).split(':').map(Some.apply).toList
        val right = a.substring(separator + 2).split(':').map(Some.apply).toList
        val middle = (0 until (8 - (left.length + right.length))).map(_ => None).toList
        left ::: middle ::: right
      } else {
        a.split(':').map(Option.apply).toList
      }).map(o => Integer.parseInt(o.getOrElse("0"), 16)).toVector
      if (v.length != 8) throw MalformedIPAddressException(address)
      apply(v(0), v(1), v(2), v(3), v(4), v(5), v(6), v(7), scope)
    }
  }

  def get(address: String): Option[IP] = try {
    address match {
      case v4(ip) => Some(ip)
      case _ if address.contains(':') => Some(v6(address))
      case _ => None
    }
  } catch {
    case _: MalformedIPAddressException => None
  }

  def unapply(address: String): Option[IP] = get(address)

  def apply(address: String): IP = get(address).getOrElse(throw MalformedIPAddressException(address))
  def apply(a: Vector[Int]): IP = if (a.length == 4) {
    v4(a(0), a(1), a(2), a(3))
  } else if (a.length == 8) {
    v6(a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7))
  } else {
    throw MalformedIPAddressException(a.toString())
  }
}

sealed trait IP {
  val address: Array[Int]
  val addressString: String

  override def equals(obj: Any): Boolean = obj match {
    case that: IP if this.addressString == that.addressString => true
    case _ => false
  }

  override def toString: String = addressString
}