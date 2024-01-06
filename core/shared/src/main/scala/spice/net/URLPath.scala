package spice.net

import fabric.rw._
import spice.net.URLPathPart._

import java.util.regex.Pattern

case class URLPath(parts: List[URLPathPart]) {
  lazy val absolute: URLPath = {
    var entries = Vector.empty[URLPathPart]
    parts.foreach {
      case UpLevel => entries = entries.dropRight(3)
      case SameLevel => entries = entries.dropRight(1)
      case part => entries = entries :+ part
    }
    URLPath(entries.toList)
  }
  lazy val encoded: String = absolute.parts.map(Encoder.apply).mkString
  lazy val decoded: String = absolute.parts.map(_.value).mkString
  lazy val asRegexString: String = absolute.parts.map {
    case URLPathPart.Argument(_) => "(.+?)"
    case part => part.value
  }.mkString
  private lazy val pattern = Pattern.compile(asRegexString)

  lazy val arguments: List[String] = parts.collect {
    case part: Argument => part.name
  }

  def withArguments(arguments: Map[String, String]): URLPath = copy(parts = parts.map {
    case part: Argument if arguments.contains(part.name) => Literal(arguments(part.name))
    case part => part
  })

  def withParams(params: (String, String)*): String = if (params.nonEmpty) {
    s"$this?${params.map(t => s"${t._1}=${t._2}").mkString("&")}"
  } else {
    toString
  }

  def extractArguments(literal: URLPath): Map[String, String] = {
    val matcher = pattern.matcher(literal.decoded)
    if (!matcher.matches()) {
      throw new RuntimeException(s"Literal path: ${literal.decoded} was not a match to $asRegexString")
    }
    if (matcher.groupCount() != arguments.length) {
      throw new RuntimeException(s"Arguments ${arguments.length} was not the same as the match groups: ${matcher.groupCount()}")
    }
    arguments.zipWithIndex.map {
      case (name, index) => name -> matcher.group(index + 1)
    }.toMap
  }

  def append(path: String): URLPath = if (path.startsWith("/")) {
    URLPath.parse(path)
  } else {
    val left = parts.dropRight(1)
    val right = URLPath.parse(path, absolutize = false)
    URLPath(left ::: right.parts)
  }

  def merge(that: URLPath): URLPath = URLPath(this.parts ::: that.parts)

  override def equals(obj: Any): Boolean = obj match {
    case that: URLPath => if (this.arguments.nonEmpty == that.arguments.nonEmpty) {
      this.toString == that.toString
    } else if (this.arguments.nonEmpty) {
      val regex = this.asRegexString
      that.decoded.matches(regex)
    } else {
      val regex = that.asRegexString
      this.decoded.matches(regex)
    }
    case _ => false
  }

  override def toString: String = encoded
}

object URLPath {
  implicit val rw: RW[URLPath] = RW.string[URLPath](
    asString = _.encoded,
    fromString = (s: String) => parse(s)
  )

  val empty: URLPath = URLPath(Nil)

  def parse(path: String, absolutize: Boolean = true): URLPath = {
    val parts = path
      .split("((?<=/)|(?=/))")
      .toList
      .flatMap(URLPathPart.apply)

    URLPath(parts) match {
      case p if absolutize => p.absolute
      case p => p
    }
  }
}