package spice.http.content

import cats.effect.IO
import fabric.Json
import fabric.io.JsonParser

import java.io.File
import spice.http.Headers
import spice.net.ContentType

case class FormDataContent(data: List[FormData]) extends Content {
  override def length: Long = -1
  override def lastModified: Long = -1
  override def contentType: ContentType = ContentType.`multipart/form-data`
  override def withContentType(contentType: ContentType): Content = this
  override def withLastModified(lastModified: Long): Content = this

  def fileOption(key: String): Option[FormDataEntry.FileEntry] = data.find(_.key == key).map(_.entries.head.asInstanceOf[FormDataEntry.FileEntry])
  def stringOption(key: String): Option[FormDataEntry.StringEntry] = data.find(_.key == key).map(_.entries.head.asInstanceOf[FormDataEntry.StringEntry])
  def file(key: String): FormDataEntry.FileEntry = fileOption(key).getOrElse(throw new RuntimeException(s"Not found: $key in $this."))
  def string(key: String): FormDataEntry.StringEntry = stringOption(key).getOrElse(throw new RuntimeException(s"Not found: $key in $this."))
  def json(key: String): Json = JsonParser(string(key).value)

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

  def withEntry(key: String, entry: FormDataEntry): FormDataContent = {
    val formData = data.find(_.key == key).getOrElse(FormData(key, Nil))
    val updated = formData.copy(entries = formData.entries ::: List(entry))
    copy(data = data.filterNot(_.key == key) ::: List(updated))
  }

  override def toString: String = s"FormDataContent(${data.map(_.key).mkString(", ")})"

  override def asString: IO[String] = IO.pure(toString)

  override def asStream: fs2.Stream[IO, Byte] = throw new UnsupportedOperationException("FormDataContent cannot be represented as a stream!")
}

object FormDataContent extends FormDataContent(Nil)