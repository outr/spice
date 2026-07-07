package spice.http.client.intercept

import rapid.Task
import spice.http.HttpRequest

import scala.concurrent.duration.*

/**
 * Client-side rate limiter. Hands out evenly-spaced send slots so that, regardless of how many
 * callers hit [[before]] concurrently, requests fire no closer together than `perRequestDelay`
 * (steady throughput of 1 / perRequestDelay). Each caller reserves the next free slot under a short
 * lock and then waits for it, so concurrent callers queue rather than bursting together.
 */
case class RateLimiter(perRequestDelay: FiniteDuration) extends InterceptorAdapter { self =>
  private val intervalMillis: Long = math.max(0L, perRequestDelay.toMillis)
  // The earliest time the NEXT request may be sent; advanced by one interval per reservation.
  private var nextSlot: Long = 0L

  override def before(request: HttpRequest): Task[HttpRequest] = Task.unit.flatMap { _ =>
    val waitMillis = self.synchronized {
      val now = System.currentTimeMillis()
      val slot = math.max(now, nextSlot)
      nextSlot = slot + intervalMillis
      slot - now
    }
    if (waitMillis > 0L) Task.sleep(waitMillis.millis).map(_ => request) else Task.pure(request)
  }
}