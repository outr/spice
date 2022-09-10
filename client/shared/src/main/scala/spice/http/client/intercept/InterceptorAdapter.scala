package spice.http.client.intercept

import cats.effect.IO
import spice.http.{HttpRequest, HttpResponse}

import scala.util.Try

abstract class InterceptorAdapter extends Interceptor {
  override def before(request: HttpRequest): IO[HttpRequest] = IO.pure(request)

  override def after(request: HttpRequest, result: Try[HttpResponse]): IO[Try[HttpResponse]] = IO.pure(result)
}