package spice.http.durable

import spice.http.Headers

/** What a [[ReceivedFile]] says of itself from the sender's envelope, on every platform. */
trait ReceivedFileHeaders {
  def headers: Headers

  /** Declared content type, if the sender provided one. */
  def contentType: Option[String] = headers.first(Headers.`Content-Type`)

  /** Declared file name, if the sender provided one (Content-Disposition `filename`). */
  def fileName: Option[String] = headers.first(Headers.Response.`Content-Disposition`)
    .flatMap { v =>
      val marker = "filename="
      val i = v.indexOf(marker)
      if (i < 0) None else Some(v.substring(i + marker.length).trim.stripPrefix("\"").stripSuffix("\""))
    }
}
