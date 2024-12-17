package spice.http.content

import fabric.define.DefType
import fabric.{Json, Null}
import fabric.io.JsonParser
import fabric.rw._
import rapid.Task

import java.io.File
import spice.http.Headers
import spice.net.ContentType

case class FormDataContent(entries: Map[String, FormDataEntry]) extends Content {
  override def length: Long = -1
  override def lastModified: Long = -1
  override def contentType: ContentType = ContentType.`multipart/form-data`
  override def withContentType(contentType: ContentType): Content = this
  override def withLastModified(lastModified: Long): Content = this

  lazy val strings: Map[String, FormDataEntry.StringEntry] = entries.collect {
    case (key, entry: FormDataEntry.StringEntry) => key -> entry
  }
  lazy val jsons: Map[String, Json] = strings.collect {
    case (key, entry) if entry.headers.contains(Headers.`Content-Type`) => key -> JsonParser(entry.value)
  }
  lazy val files: Map[String, FormDataEntry.FileEntry] = entries.collect {
    case (key, entry: FormDataEntry.FileEntry) => key -> entry
  }

  def getFile(key: String): FormDataEntry.FileEntry = files.getOrElse(key, throw new RuntimeException(s"Not found: $key in $this."))
  def getString(key: String): FormDataEntry.StringEntry = strings.getOrElse(key, throw new RuntimeException(s"Not found: $key in $this."))
  def getJson(key: String): Json = jsons.getOrElse(key, throw new RuntimeException(s"Not found: $key in $this."))

  def withFile(key: String, fileName: String, file: File, headers: Headers = Headers.empty): FormDataContent = {
    assert(file.exists(), s"Unable to find file: ${file.getAbsolutePath}")
    val entry = FormDataEntry.FileEntry(fileName, file, headers)
    withEntry(key, entry)
  }

  def withString(key: String, value: String, headers: Headers = Headers.empty): FormDataContent = {
    val entry = FormDataEntry.StringEntry(value, headers)
    withEntry(key, entry)
  }

  def withJson(key: String, value: Json, headers: Headers = Headers.empty): FormDataContent = {
    val entry = FormDataEntry.JsonEntry(value, headers)
    withEntry(key, entry)
  }

  def withEntry(key: String, entry: FormDataEntry, replace: Boolean = false): FormDataContent = {
    if (!replace && entries.contains(key)) throw new RuntimeException(s"Key $key already exists in the form data!")
    copy(entries + (key -> entry))
  }

  override def toString: String = s"FormDataContent(${entries.map(t => s"${t._1}: ${t._2}")})"

  override def asString: Task[String] = Task.pure(toString)

  override def asStream: rapid.Stream[Byte] = throw new UnsupportedOperationException("FormDataContent cannot be represented as a stream!")
}

object FormDataContent extends FormDataContent(Map.empty) {
  implicit val rw: RW[FormDataContent] = RW.from[FormDataContent](
    r = _ => Null,
    w = _ => this,
    d = DefType.Null
  )
}