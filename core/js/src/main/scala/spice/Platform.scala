package spice

import scala.scalajs.js

object Platform {
  def parseHTTPDate(date: String): Option[Long] = {
    val millis = js.Date.parse(date)
    if (millis.isNaN) None else Some(millis.toLong)
  }

  def toHTTPDate(time: Long): String = {
    new js.Date(time.toDouble).toUTCString()
  }
}
