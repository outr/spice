package spice.net

case class IPv6(a: Int = 0,
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

  override val address: Vector[Int] = Vector(a, b, c, d, e, f, g, h)
  override val addressString: String = s"${nc(a)}:${nc(b)}:${nc(c)}:${nc(d)}:${nc(e)}:${nc(f)}:${nc(g)}:${nc(h)}${scope.getOrElse("")}"

  lazy val canonicalString: String = s"${cs(a)}:${cs(b)}:${cs(c)}:${cs(d)}:${cs(e)}:${cs(f)}:${cs(g)}:${cs(h)}${scope.getOrElse("")}"
}

object IPv6 {
  lazy val LocalHost: IPv6 = IPv6()

  def apply(address: String): IPv6 = {
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