package spice.net

import fabric.rw._
import spice.UserException

import scala.util.Try
import scala.util.matching.Regex

sealed trait IP {
  val address: Vector[Int]
  val addressString: String

  override def equals(obj: Any): Boolean = obj match {
    case that: IP if this.addressString == that.addressString => true
    case _ => false
  }

  override def toString: String = addressString
}

object IP {
  implicit val rw: RW[IP] = RW.string[IP](
    asString = (ip: IP) => ip.addressString,
    fromString = (s: String) => fromString(s).getOrElse(throw UserException(s"Invalid IP address: $s"))
  )

  case class v4(a: Int = 127, b: Int = 0, c: Int = 0, d: Int = 1) extends IP {
    override val address: Vector[Int] = Vector(a, b, c, d)
    override val addressString: String = s"$a.$b.$c.$d"
  }

  object v4 {
    lazy val LocalHost: v4 = v4()

    private val Regex: Regex = """\b(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b""".r

    def unapply(address: String): Option[v4] = fromAddress(address)

    def fromAddress(address: String): Option[v4] = address match {
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
    private def nc(i: Int): String = i.toHexString

    private def cs(i: Int): String = f"$i%04x"

    private lazy val s: String = scope.map(s => s"%$s").getOrElse("")

    override val address: Vector[Int] = Vector(a, b, c, d, e, f, g, h)
    override val addressString: String = s"${nc(a)}:${nc(b)}:${nc(c)}:${nc(d)}:${nc(e)}:${nc(f)}:${nc(g)}:${nc(h)}$s"

    lazy val canonicalString: String = s"${cs(a)}:${cs(b)}:${cs(c)}:${cs(d)}:${cs(e)}:${cs(f)}:${cs(g)}:${cs(h)}$s"
  }

  object v6 {
    lazy val LocalHost: v6 = v6()

    def unapply(address: String): Option[v6] = fromAddress(address)

    def fromAddress(address: String): Option[v6] = {
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
      }).flatMap(o => Try(Integer.parseInt(o.getOrElse("0"), 16)).toOption).toVector
      if (v.length != 8) {
        None
      } else {
        Some(apply(v(0), v(1), v(2), v(3), v(4), v(5), v(6), v(7), scope))
      }
    }
  }

  def fromString(address: String): Option[IP] = v4.fromAddress(address).orElse(v6.fromAddress(address))
}