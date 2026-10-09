package spice.http.client

import rapid.*
import spice.http.{HttpMethod, HttpRequest, HttpResponse}

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}

trait RetryManager {
  def retry(request: HttpRequest,
            retry: Task[Try[HttpResponse]],
            failures: Int,
            throwable: Throwable): Task[Try[HttpResponse]]

  /** Whether a failed request may be sent again at all: one whose method is idempotent, or one that failed before a
    * connection was made. A POST or PATCH that timed out may already have been acted on, so it is not sent again.
    * Override to resend such requests, for example ones carrying an idempotency key. */
  def retryable(request: HttpRequest, throwable: Throwable): Boolean =
    RetryManager.idempotent(request.method) || ClientPlatform.failedBeforeConnect(throwable)
}

object RetryManager {
  /** The methods RFC 9110 §9.2.2 defines as idempotent. */
  def idempotent(method: HttpMethod): Boolean = method match {
    case HttpMethod.Get | HttpMethod.Head | HttpMethod.Options | HttpMethod.Trace | HttpMethod.Put |
        HttpMethod.Delete => true
    case _ => false
  }

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
      case Some(d) =>
        val isTimeout = throwable.isInstanceOf[java.util.concurrent.TimeoutException] ||
          throwable.getClass.getName == "java.net.SocketTimeoutException"
        val logTask = Task {
          if (warnRetries) {
            if (isTimeout) {
              scribe.warn(s"Request to ${request.url} timed out (${throwable.getLocalizedMessage}, failures: $failures). Retrying after $d...")
            } else {
              scribe.warn(s"Request to ${request.url} failed (${throwable.getLocalizedMessage}, ${throwable.getClass.getSimpleName}, failures: $failures). Retrying after $d...")
            }
          }
        }
        for {
          _ <- logTask
          _ <- Task.sleep(d)
          response <- retry
        } yield response
      case None =>
        scribe.error(s"Request to ${request.url} permanently failed (${throwable.getMessage}, failures: $failures).")
        Task.pure(Failure(throwable))
    }
  }
}