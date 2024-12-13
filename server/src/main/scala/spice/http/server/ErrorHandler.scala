package spice.http.server

import rapid.Task
import spice.http.HttpExchange

trait ErrorHandler {
  def handle(exchange: HttpExchange, t: Option[Throwable]): Task[HttpExchange]
}
