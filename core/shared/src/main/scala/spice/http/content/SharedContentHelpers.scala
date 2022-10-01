package spice.http.content

import fabric.io.JsonFormatter
import fabric.rw.Reader
import fabric.{Json, Null, obj, str}
import spice.net.ContentType

import java.io.File
import java.net.URL

trait SharedContentHelpers {
  val empty: Content = string("", ContentType.`text/plain`)
  lazy val form: FormDataContent = FormDataContent(Nil)

  def graphql(query: String, operationName: Option[String] = None, variables: List[(String, Json)] = Nil): Content = {
    json(obj(
      "query" -> str(query),
      "operationName" -> operationName.map(str).getOrElse(Null),
      "variables" -> obj(variables: _*)
    ))
  }

  def json(value: Json, compact: Boolean = true): Content = JsonContent(value, compact)

  def jsonFrom[T: Reader](value: T, compact: Boolean = true): Content = JsonContent.from(value, compact)

  def string(value: String, contentType: ContentType): Content = StringContent(value, contentType)

  def bytes(value: Array[Byte], contentType: ContentType): Content = BytesContent(value, contentType)

  def classPath(path: String): Content = classPathOption(path).getOrElse(throw new RuntimeException(s"Invalid URL or not found in class-loader: $path."))

  def classPathOption(path: String): Option[Content] = {
    val o = Option(Thread.currentThread().getContextClassLoader.getResource(path))
    o.map(classPath)
  }

  def file(file: File): Content

  def file(file: File, contentType: ContentType): Content

  def url(url: URL): Content

  def url(url: URL, contentType: ContentType): Content

  def classPath(url: URL): Content

  def classPath(path: String, contentType: ContentType): Content
}
