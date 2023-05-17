package spice.http.client

import cats.effect.IO
import spice.http.{HttpRequest, HttpResponse}

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}
import scribe.cats.{io => logger}

trait RetryManager {
  def retry(request: HttpRequest,
            retry: IO[Try[HttpResponse]],
            failures: Int,
            throwable: Throwable): IO[Try[HttpResponse]]
}

object RetryManager {
  def none: RetryManager = apply(_ => None)

  def simple(retries: Int, delay: FiniteDuration): RetryManager = apply { failures =>
    if (failures > retries) {
      None
    } else {
      Some(delay)
    }
  }

  def delays(delays: FiniteDuration*): RetryManager = {
    val v = delays.toVector
    apply { failures =>
      if (failures > v.length) {
        None
      } else {
        Some(v(failures - 1))
      }
    }
  }

  def apply(f: Int => Option[FiniteDuration]): RetryManager = Advanced(f)

  case class Simple(retries: Int, delay: FiniteDuration) extends RetryManager {
    override def retry(request: HttpRequest,
                       retry: IO[Try[HttpResponse]],
                       failures: Int,
                       throwable: Throwable): IO[Try[HttpResponse]] = {
      if (failures > retries) {
        IO.pure(Failure(throwable))
      } else {
        for {
          _ <- logger.warn(s"Request to ${request.url} failed (${throwable.getMessage}). Retrying after $delay...")
          _ <- IO.sleep(delay)
          response <- retry
        } yield response
      }
    }
  }

  case class Delays(delays: FiniteDuration*) extends RetryManager {
    private val v = delays.toVector

    override def retry(request: HttpRequest,
                       retry: IO[Try[HttpResponse]],
                       failures: Int,
                       throwable: Throwable): IO[Try[HttpResponse]] = {
      if (failures > v.length) {
        IO.pure(Failure(throwable))
      } else {
        IO.sleep(v(failures - 1)).flatMap(_ => retry)
      }
    }
  }

  case class Advanced(delay: Int => Option[FiniteDuration]) extends RetryManager {
    override def retry(request: HttpRequest,
                       retry: IO[Try[HttpResponse]],
                       failures: Int,
                       throwable: Throwable): IO[Try[HttpResponse]] = delay(failures) match {
      case Some(d) => for {
        _ <- logger.warn(s"Request to ${request.url} failed (${throwable.getMessage}, failures: $failures). Retrying after $d...")
        _ <- IO.sleep(d)
        response <- retry
      } yield response
      case None =>
        logger.error(s"Request to ${request.url} permanently failed (${throwable.getMessage}, failures: $failures).")
        IO.pure(Failure(throwable))
    }
  }
}