package spice.net

import scala.util.matching.Regex

object Decoder {
  private val encodedRegex = """%([a-zA-Z0-9]{2})""".r

  def apply(s: String): String = try {
    encodedRegex.replaceAllIn(s.replace("\\", "\\\\"), (m: Regex.Match) => {
      val g = m.group(1)
      val code = Integer.parseInt(g, 16)
      val c = code.toChar
      if (c == '\\') {
        "\\\\"
      } else {
        c.toString
      }
    })
  } catch {
    case t: Throwable => throw new RuntimeException(s"Failed to decode: [$s]", t)
  }
}
