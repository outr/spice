package spice.http.client

import cats.effect.IO
import spice.http.content.Content
import spice.http.{HttpRequest, HttpResponse}

import scala.util.Try

abstract class HttpClientImplementation(val config: HttpClientConfig) {
  HttpClientImplementation.register(this)

  def send(request: HttpRequest): IO[Try[HttpResponse]]

  def content2String(content: Content): String
}

object HttpClientImplementation {
  private var instance: Option[HttpClientImplementation] = None

  def apply(): HttpClientImplementation = instance.getOrElse(throw new RuntimeException(s"No HttpClientImplementation is registered. Make sure Moduload.init() is called first."))

  def register(implementation: HttpClientImplementation): Unit = synchronized {
    if (instance.nonEmpty) throw new RuntimeException(s"HttpClientImplementation already defined! Attempting to register: ${implementation.getClass.getName}, but ${instance.get.getClass.getName} is already registered.")
    instance = Some(implementation)
  }
}