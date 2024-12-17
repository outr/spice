package spice.http.client

import rapid.Task
import spice.http.{HttpRequest, HttpResponse}

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}
import scribe.{rapid => logger}

trait RetryManager {
  def retry(request: HttpRequest,
            retry: Task[Try[HttpResponse]],
            failures: Int,
            throwable: Throwable): Task[Try[HttpResponse]]
}

object RetryManager {
  def none: RetryManager = apply(warnRetries = true)(_ => None)

  def simple(retries: Int, delay: FiniteDuration, warnRetries: Boolean = true): RetryManager = apply(warnRetries) { failures =>
    if (failures > retries) {
      None
    } else {
      Some(delay)
    }
  }

  def delays(warnRetries: Boolean, delays: FiniteDuration*): RetryManager = {
    val v = delays.toVector
    apply(warnRetries) { failures =>
      if (failures > v.length) {
        None
      } else {
        Some(v(failures - 1))
      }
    }
  }

  def apply(warnRetries: Boolean)(f: Int => Option[FiniteDuration]): RetryManager = Standard(f, warnRetries)

  case class Standard(delay: Int => Option[FiniteDuration], warnRetries: Boolean) extends RetryManager {
    override def retry(request: HttpRequest,
                       retry: Task[Try[HttpResponse]],
                       failures: Int,
                       throwable: Throwable): Task[Try[HttpResponse]] = delay(failures) match {
      case Some(d) => for {
        _ <- logger.warn(s"Request to ${request.url} failed (${throwable.getMessage}, failures: $failures). Retrying after $d...").when(warnRetries)
        _ <- Task.sleep(d)
        response <- retry
      } yield response
      case None =>
        logger.error(s"Request to ${request.url} permanently failed (${throwable.getMessage}, failures: $failures).")
        Task.pure(Failure(throwable))
    }
  }
}