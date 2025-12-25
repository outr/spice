package spice.http.client.intercept

import rapid.Task
import spice.http.HttpRequest

import scala.concurrent.duration._

// use per: Int, elapsed: Long - List[Long]
case class RateLimiter(perRequestDelay: FiniteDuration) extends InterceptorAdapter { self =>
  private val maxDelay = perRequestDelay.toMillis
  @volatile private var lastTime: Long = 0L

  override def before(request: HttpRequest): Task[HttpRequest] = Task.unit.flatMap { _ =>
    self.synchronized {
      val now = System.currentTimeMillis()
      val delay = (lastTime + maxDelay) - now
      if (delay > 0L) {
        lastTime = now
        Task.sleep(delay.millis).map(_ => request)
      } else {
        Task.pure(request)
      }
    }
  }
}