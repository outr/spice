package spice.http

import cats.effect.IO

case class HttpExchange(request: HttpRequest,
                        response: HttpResponse,
                        finished: Boolean = false) {
  def modify(f: HttpResponse => IO[HttpResponse]): IO[HttpExchange] = {
    f(response).map(r => copy(response = r))
  }

  def finish(): HttpExchange = copy(finished = true)
}