package spice.net

sealed trait PathPart extends Any {
  def value: String
}

object PathPart {
  private val ArgumentPartRegex1 = """[:](.+)""".r
  private val ArgumentPartRegex2 = """[{](.+)[}]""".r

  object UpLevel extends PathPart {
    override def value: String = ".."
  }

  object SameLevel extends PathPart {
    override def value: String = "."
  }

  class Literal(val value: String) extends AnyVal with PathPart

  class Argument(val name: String) extends AnyVal with PathPart {
    override def value: String = s":$name"
  }

  def apply(value: String): Option[PathPart] = value match {
    case null | "" => None
    case ".." => Some(UpLevel)
    case "." => Some(SameLevel)
    case ArgumentPartRegex1(name) => Some(new Argument(name))
    case ArgumentPartRegex2(name) => Some(new Argument(name))
    case s => Some(new Literal(s))
  }

  def equals(p1: PathPart, p2: PathPart): Boolean = if (p1 == p2) {
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