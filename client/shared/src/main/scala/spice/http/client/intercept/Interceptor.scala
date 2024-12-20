package spice.http.client.intercept

import rapid.Task
import spice.http.{HttpRequest, HttpResponse}

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

trait Interceptor {
  def before(request: HttpRequest): Task[HttpRequest]

  def after(request: HttpRequest, result: Try[HttpResponse]): Task[Try[HttpResponse]]
}

object Interceptor {
  object empty extends InterceptorAdapter

  def apply(interceptors: Interceptor*): Interceptor = MultiInterceptor(interceptors.toList)

  def rateLimited(perRequestDelay: FiniteDuration): Interceptor = RateLimiter(perRequestDelay)
}