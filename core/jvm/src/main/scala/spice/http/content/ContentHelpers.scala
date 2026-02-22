package spice.http.content

import spice.net.ContentType

import java.io.File
import java.net.URL
import scala.language.implicitConversions

trait ContentHelpers extends SharedContentHelpers {
  given Conversion[File, Content] = file => this.file(file)
  given Conversion[java.net.URL, Content] = url => this.url(url)

  override def file(file: File): Content = FileContent(file, ContentType.byFileName(file.getName))
  override def file(file: File, contentType: ContentType): Content = FileContent(file, contentType)
  override def url(url: URL): Content = URLContent(url, ContentType.byFileName(url.toString))
  override def url(url: URL, contentType: ContentType): Content = URLContent(url, contentType)
  override def classPath(url: URL): Content = URLContent(url, ContentType.byFileName(url.toString))
  override def classPath(path: String, contentType: ContentType): Content = URLContent(Thread.currentThread().getContextClassLoader.getResource(path), contentType)
}