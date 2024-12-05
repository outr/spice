package spice.net

import com.sun.org.apache.xalan.internal.xsltc.compiler.sym

sealed trait URLPathPart extends Any {
  def value: String

  override def toString: String = value
}

object URLPathPart {
  private val ArgumentPartRegex1 = """:(.+)""".r
  private val ArgumentPartRegex2 = """[{](.+)[}]""".r

  object Separator extends URLPathPart {
    override def value: String = "/"
  }

  object UpLevel extends URLPathPart {
    override def value: String = ".."
  }

  object SameLevel extends URLPathPart {
    override def value: String = "."
  }

  case class Literal(value: String) extends AnyVal with URLPathPart

  case class Argument(name: String) extends AnyVal with URLPathPart {
    override def value: String = s"{$name}"
  }

  final def apply(value: String): List[URLPathPart] = value match {
    case null | "" => Nil
    case "/" => List(Separator)
    case ".." => List(UpLevel)
    case "." => List(SameLevel)
    case ArgumentPartRegex1(name) => List(Argument(name))
    case ArgumentPartRegex2(name) => List(Argument(name))
    case encoded =>
      val s = Decoder(encoded)
//      val colonIndex = s.indexOf(':')
      val openBraceIndex = s.indexOf('{')
      val closeBraceIndex = s.indexOf('}', math.max(openBraceIndex, 0))
//      if (colonIndex != -1) {
//        val pre = s.substring(0, colonIndex)
//        apply(pre) ::: List(Argument(s.substring(colonIndex + 1)))
//      } else
      if (openBraceIndex != -1 && closeBraceIndex != -1) {
        val pre = s.substring(0, openBraceIndex)
        val post = s.substring(closeBraceIndex + 1)
        val arg = Argument(s.substring(openBraceIndex + 1, closeBraceIndex))
        apply(pre) ::: List(arg) ::: apply(post)
      } else {
        List(Literal(s))
      }
  }

  def equals(p1: URLPathPart, p2: URLPathPart): Boolean = if (p1 == p2) {
    true
  } else {
    p1 match {
      case _: Literal => p2 match {
        case _: Argument => true
        case _ => false
      }
      case _: Argument => p2 match {
        case _: Literal => true
        case _ => false
      }
      case _ => false
    }
  }
}