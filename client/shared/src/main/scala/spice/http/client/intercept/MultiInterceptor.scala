package spice.http.client.intercept

import rapid.Task
import spice.http.{HttpRequest, HttpResponse}

import scala.util.Try

case class MultiInterceptor(interceptors: List[Interceptor]) extends Interceptor {
  override def before(request: HttpRequest): Task[HttpRequest] = beforeRecursive(request, interceptors)

  private def beforeRecursive(request: HttpRequest, list: List[Interceptor]): Task[HttpRequest] = if (list.isEmpty) {
    Task.pure(request)
  } else {
    val interceptor = list.head
    interceptor.before(request).flatMap { updated =>
      beforeRecursive(updated, list.tail)
    }
  }

  override def after(request: HttpRequest, result: Try[HttpResponse]): Task[Try[HttpResponse]] = {
    afterRecursive(request, result, interceptors)
  }

  private def afterRecursive(request: HttpRequest,
                             result: Try[HttpResponse],
                             list: List[Interceptor]): Task[Try[HttpResponse]] = if (list.isEmpty) {
    Task.pure(result)
  } else {
    val interceptor = list.head
    interceptor.after(request, result).flatMap { updated =>
      afterRecursive(request, updated, list.tail)
    }
  }
}
