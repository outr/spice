package spice.http.client

import cats.effect.IO
import spice.http.content.Content
import spice.http.{HttpRequest, HttpResponse}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

abstract class HttpClientImplementation(val config: HttpClientConfig) {
  def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool

  def send(request: HttpRequest): IO[Try[HttpResponse]]

  def content2String(content: Content): String

  def dispose(): IO[Unit]
}