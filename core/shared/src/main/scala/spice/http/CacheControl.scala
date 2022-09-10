package spice.http

sealed abstract class CacheControl {
  def value: String
}

object CacheControl extends MultiTypedHeaderKey[CacheControl] {
  private val MaxAgeRegex = """max-age=(\d+)""".r

  override def key: String = "Cache-Control"
  override protected def commaSeparated: Boolean = false

  override def value(headers: Headers): List[CacheControl] = headers.get(this).mkString(", ").split(',').map(_.toLowerCase.trim).map {
    case "public" => Public
    case "private" => Private
    case "no-cache" => NoCache
    case "must-revalidate" => MustRevalidate
    case "no-store" => NoStore
    case MaxAgeRegex(seconds) => MaxAge(seconds.toLong)
  }.toList

  override def apply(values: CacheControl*): Header = Header(this, values.map(_.value).mkString(", "))

  case object Public extends CacheControl {
    override def value: String = "public"
  }
  case object Private extends CacheControl {
    override def value: String = "private"
  }
  case object NoCache extends CacheControl {
    override def value: String = "no-cache"
  }
  case object MustRevalidate extends CacheControl {
    override def value: String = "must-revalidate"
  }
  case object NoStore extends CacheControl {
    override def value: String = "no-store"
  }
  case class MaxAge(seconds: Long) extends CacheControl {
    override def value: String = s"max-age=$seconds"
  }
}