package spice.net

import java.util.regex.{Matcher, Pattern}
import scala.util.matching.Regex

object Decoder {
  private val encodedRegex = """%([a-zA-Z0-9]{2})""".r

  def apply(s: String): String = {
    val cleaned = s.replace("\\", "\\\\")
    try {
      encodedRegex.replaceAllIn(cleaned, (m: Regex.Match) => {
        val g = m.group(1)
        val code = Integer.parseInt(g, 16)
        val c = code.toChar
        if (c == '\\') {
          "\\\\"
        } else {
          Matcher.quoteReplacement(c.toString)
        }
      })
    } catch {
      case t: Throwable => throw new RuntimeException(s"Failed to decode: [$cleaned]", t)
    }
  }
}
