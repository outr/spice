package spice.net

import fabric.rw.RW
import spice.net.PathPart._

case class Path(parts: List[PathPart]) {
  lazy val absolute: Path = {
    var entries = Vector.empty[PathPart]
    parts.foreach {
      case UpLevel => entries = entries.dropRight(1)
      case SameLevel => // Ignore
      case part => entries = entries :+ part
    }
    Path(entries.toList)
  }
  lazy val encoded: String = absolute.parts.map(_.value).map(Encoder.apply).mkString("/", "/", "")
  lazy val decoded: String = absolute.parts.map(_.value).mkString("/", "/", "")

  lazy val arguments: List[String] = parts.collect {
    case part: Argument => part.name
  }

  def withArguments(arguments: Map[String, String]): Path = copy(parts = parts.map {
    case part: Argument if arguments.contains(part.name) => Literal(arguments(part.name))
    case part => part
  })

  def withParams(params: (String, String)*): String = if (params.nonEmpty) {
    s"$this?${params.map(t => s"${t._1}=${t._2}").mkString("&")}"
  } else {
    toString
  }

  def extractArguments(literal: Path): Map[String, String] = {
    assert(parts.length == literal.parts.length, s"Literal path must have the same number of parts as the one being extracted for")
    parts.zip(literal.parts).flatMap {
      case (p1, p2) => p1 match {
        case ap: Argument => Some(ap.name -> p2.value)
        case _ => None
      }
    }.toMap
  }

  def append(path: String): Path = if (path.startsWith("/")) {
    Path.parse(path)
  } else {
    val left = parts.dropRight(1)
    val right = Path.parse(path, absolutize = false)
    Path(left ::: right.parts)
  }

  def merge(that: Path): Path = Path(this.parts ::: that.parts)

  override def equals(obj: Any): Boolean = obj match {
    case that: Path if this.parts.length == that.parts.length =>
      this.parts.zip(that.parts).forall {
        case (thisPart, thatPart) => PathPart.equals(thisPart, thatPart)
      }
    case _ => false
  }

  override def toString: String = encoded
}

object Path {
  implicit val rw: RW[Path] = RW.string[Path](
    asString = _.encoded,
    fromString = (s: String) => parse(s)
  )

  val empty: Path = Path(Nil)

  def parse(path: String, absolutize: Boolean = true): Path = {
    val updated = if (path.startsWith("/")) {
      path.substring(1)
    } else {
      path
    }
    val parts = updated.split('/').toList.map(Decoder.apply).flatMap(PathPart.apply)
    Path(parts) match {
      case p if absolutize => p.absolute
      case p => p
    }
  }
}