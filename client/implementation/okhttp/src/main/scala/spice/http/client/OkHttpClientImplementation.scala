package spice.http.client

import cats.effect.unsafe.implicits.global
import cats.effect.IO
import moduload.Moduload
import spice.http._
import spice.http.content._
import spice.streamer._

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

object OkHttpClientImplementation extends Moduload with HttpClientImplementation {
  private[client] val _total = new AtomicLong(0L)
  private[client] val _active = new AtomicLong(0L)
  private[client] val _successful = new AtomicLong(0L)
  private[client] val _failure = new AtomicLong(0L)

  override def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool =
    OkHttpConnectionPool(maxIdleConnections, keepAlive)

  override def content2String(content: Content): String = content match {
    case c: StringContent => c.value
    case c: BytesContent => String.valueOf(c.value)
    case c: FileContent => Streamer(c.file, new mutable.StringBuilder).unsafeRunSync().toString
    case _ => throw new RuntimeException(s"$content not supported")
  }

  override protected def createInstance(client: HttpClient): HttpClientInstance = new OkHttpClientInstance(client)

  override def load(): Unit = {
    HttpClientImplementationManager.register(_ => this)
  }

  override def error(t: Throwable): Unit = {
    scribe.error("Error while attempting to register OkHttpClientImplementation", t)
  }

  private[client] def process(io: IO[Try[HttpResponse]]): IO[Try[HttpResponse]] = {
    _total.incrementAndGet()
    _active.incrementAndGet()
    io.flatMap { t =>
      IO {
        t match {
          case Success(_) =>
            _successful.incrementAndGet()
            _active.decrementAndGet()
          case Failure(_) =>
            _failure.incrementAndGet()
            _active.decrementAndGet()
        }
      }.map(_ => t)
    }
  }

  def total: Long = _total.get()
  def active: Long = _active.get()
  def successful: Long = _successful.get()
  def failure: Long = _failure.get()
}