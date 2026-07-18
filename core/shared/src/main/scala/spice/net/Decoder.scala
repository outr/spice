package spice.net

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

object Decoder {
  private def isHex(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  /**
   * Percent-decode `s` per RFC 3986: a run of `%XX` escapes is a byte sequence decoded as UTF-8, so a
   * multi-byte character (e.g. `%C3%A9` -> 'é') round-trips a web client's encoding. Each `%XX` was
   * previously turned into a single char equal to the byte (ISO-8859-1), which mangled every
   * non-ASCII value into mojibake. Literal characters, including '+', pass through unchanged; a
   * malformed escape (a '%' not followed by two hex digits) is left literal.
   */
  def apply(s: String): String = {
    val out = new StringBuilder(s.length)
    val bytes = new ByteArrayOutputStream()
    def flushBytes(): Unit = if (bytes.size() > 0) {
      out.append(new String(bytes.toByteArray, StandardCharsets.UTF_8))
      bytes.reset()
    }
    var i = 0
    val n = s.length
    while (i < n) {
      val c = s.charAt(i)
      if (c == '%' && i + 2 < n && isHex(s.charAt(i + 1)) && isHex(s.charAt(i + 2))) {
        bytes.write(Integer.parseInt(s.substring(i + 1, i + 3), 16))
        i += 3
      } else {
        flushBytes()
        out.append(c)
        i += 1
      }
    }
    flushBytes()
    out.toString
  }
}
