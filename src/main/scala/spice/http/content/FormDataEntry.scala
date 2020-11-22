package spice.http.content

import java.io.File

sealed trait FormDataEntry {
  def headers: Headers
}

case class FileEntry(fileName: String, file: File, headers: Headers) extends FormDataEntry

case class StringEntry(value: String, headers: Headers) extends FormDataEntry