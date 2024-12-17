package spice.http.client.intercept

import rapid.Task
import spice.http.{HttpRequest, HttpResponse}

import scala.util.Try

abstract class InterceptorAdapter extends Interceptor {
  override def before(request: HttpRequest): Task[HttpRequest] = Task.pure(request)

  override def after(request: HttpRequest, result: Try[HttpResponse]): Task[Try[HttpResponse]] = Task.pure(result)
}