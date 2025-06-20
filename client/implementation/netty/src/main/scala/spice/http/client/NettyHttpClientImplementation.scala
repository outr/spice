package spice.http.client

import moduload.Moduload
import rapid.Task
import spice.http._
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

object NettyHttpClientImplementation extends Moduload with HttpClientImplementation {
  // Statistics tracking like OkHttp
  private[client] val _total = new AtomicLong(0L)
  private[client] val _active = new AtomicLong(0L)
  private[client] val _successful = new AtomicLong(0L)
  private[client] val _failure = new AtomicLong(0L)

  override def connectionPool(maxIdleConnections: Int, keepAlive: FiniteDuration): ConnectionPool =
    NettyConnectionPool(maxIdleConnections, keepAlive)

  override protected def createInstance(client: HttpClient): HttpClientInstance =
    new NettyHttpClientInstance(client) {
      // Override send to add statistics tracking
      override def send(request: HttpRequest): Task[Try[HttpResponse]] = {
        process(super.send(request))
      }
    }

  override def load(): Unit = {
    HttpClientImplementationManager.register(_ => this)
  }

  override def error(t: Throwable): Unit = {
    scribe.error("Error while registering NettyHttpClientImplementation", t)
  }

  private[client] def process(task: Task[Try[HttpResponse]]): Task[Try[HttpResponse]] = {
    _total.incrementAndGet()
    _active.incrementAndGet()
    task.flatMap { t =>
      Task {
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