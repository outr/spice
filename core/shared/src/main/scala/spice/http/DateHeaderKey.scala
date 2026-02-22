package spice.http

class DateHeaderKey(val key: String, val commaSeparated: Boolean = false) extends TypedHeaderKey[Long] {
  import DateHeaderKey.*

  override def value(headers: Headers): Option[Long] = get(headers).flatMap(parse)

  override def apply(date: Long): Header = Header(this, format(date))
}

object DateHeaderKey {
  def parse(date: String): Option[Long] = spice.Platform.parseHTTPDate(date)

  def format(date: Long): String = spice.Platform.toHTTPDate(date)
}