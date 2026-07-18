package spice.net

import java.nio.charset.StandardCharsets

object Encoder {
  // RFC 3986 pchar: unreserved / sub-delims / ":" / "@"
  // unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
  // sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
  // Also includes "{" / "}" for URI template support
  private val unreservedCharacters = Set(
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '-', '_', '.', '~',                       // unreserved
    '!', '$', '&', '\'', '(', ')', '*', '+',  // sub-delims
    ';', '=',                                   // sub-delims (continued)
    ':', '@',                                  // pchar extras
    '{', '}'                                   // URI template support
  )
  
  // Percent-encode per RFC 3986: an unreserved char passes through; anything else is emitted as its
  // UTF-8 bytes, each %XX. Iterated by CODE POINT so astral chars (emoji) encode as one UTF-8
  // sequence rather than two broken surrogate halves. Previously each char became a single %XX of its
  // code value (ISO-8859-1, and outright invalid above U+00FF) — the encode-side twin of the decoder
  // bug, so spice round-tripped with itself but disagreed with every UTF-8 web client.
  def apply(s: String): String = {
    val b = new StringBuilder(s.length)
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (unreservedCharacters.contains(c)) {
        b.append(c)
        i += 1
      } else {
        val cp = s.codePointAt(i)
        new String(Character.toChars(cp)).getBytes(StandardCharsets.UTF_8).foreach { byte =>
          b.append(f"%%${byte & 0xff}%02X")
        }
        i += Character.charCount(cp)
      }
    }
    b.toString
  }

  def apply(part: URLPathPart): String = part match {
    case URLPathPart.Separator => "/"
    case _ => apply(part.value)
  }
}