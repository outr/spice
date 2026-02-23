package spice.net

import scala.util.matching.Regex

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
    ',', ';', '=',                             // sub-delims (continued)
    ':', '@',                                  // pchar extras
    '{', '}'                                   // URI template support
  )
  
  def apply(s: String): String = s.map {
    case c if unreservedCharacters.contains(c) => c
    case c => s"%${c.toLong.toHexString.toUpperCase}"
  }.mkString

  def apply(part: URLPathPart): String = part match {
    case URLPathPart.Separator => "/"
    case _ => apply(part.value)
  }
}