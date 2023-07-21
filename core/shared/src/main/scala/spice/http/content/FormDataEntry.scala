package spice.http.content

import fabric.Json
import fabric.io.JsonFormatter

import java.io.File
import spice.http.Headers
import spice.net.ContentType

sealed trait FormDataEntry {
  def headers: Headers
}

object FormDataEntry {
  case class FileEntry(fileName: String, file: File, headers: Headers) extends FormDataEntry
  case class StringEntry(value: String, headers: Headers) extends FormDataEntry
  def JsonEntry(value: Json, headers: Headers): FormDataEntry = StringEntry(
    value = JsonFormatter.Default(value),
    headers = headers.withHeader(Headers.`Content-Type`(ContentType.`application/json`))
  )
}