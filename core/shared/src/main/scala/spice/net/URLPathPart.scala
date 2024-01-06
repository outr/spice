package spice.net

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

  def apply(value: String): Option[URLPathPart] = value match {
    case null | "" => None
    case "/" => Some(Separator)
    case ".." => Some(UpLevel)
    case "." => Some(SameLevel)
    case ArgumentPartRegex1(name) => Some(Argument(name))
    case ArgumentPartRegex2(name) => Some(Argument(name))
    case s => Some(Literal(s))
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