package spice.http

case class HttpConnection(request: HttpRequest,
                          response: HttpResponse,
                          finished: Boolean = false) {
  def modify(f: HttpResponse => HttpResponse): HttpConnection = {
    copy(response = f(response))
  }

  def finish(): HttpConnection = copy(finished = true)
}