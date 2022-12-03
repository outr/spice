package spice.http

import cats.effect.IO
import spice.http.content.Content
import spice.store.{MapStore, Store}

case class HttpExchange(request: HttpRequest,
                        response: HttpResponse = HttpResponse(),
                        store: Store = new MapStore(),
                        finished: Boolean = false) {
  def modify(f: HttpResponse => IO[HttpResponse]): IO[HttpExchange] = {
    f(response).map(r => copy(response = r))
  }

  def withContent(content: Content): IO[HttpExchange] = modify { response =>
    IO.pure(response.withContent(content))
  }

  def finish(): HttpExchange = copy(finished = true)
}