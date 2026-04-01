package spice

import java.time.{Instant, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Locale

object Platform {
  val defaultUserAgent: Option[String] = Some("Spice-HttpClient")

  private val HttpDateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("EEE, dd MMMM yyyy HH:mm:ss zzz", Locale.ENGLISH)
      .withZone(ZoneId.of("GMT"))

  def parseHTTPDate(date: String): Option[Long] = try {
    val parsed = ZonedDateTime.parse(date.replace('-', ' '), HttpDateFormatter)
    Some(parsed.toInstant.toEpochMilli)
  } catch {
    case t: Throwable =>
      scribe.warn(s"Unable to parse date header: $date (${t.getMessage})")
      None
  }

  def toHTTPDate(time: Long): String = HttpDateFormatter.format(Instant.ofEpochMilli(time))
}
