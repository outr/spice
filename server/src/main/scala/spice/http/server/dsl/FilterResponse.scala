package spice.http.server.dsl

import spice.http.HttpExchange

sealed trait FilterResponse {
  def exchange: HttpExchange
}

object FilterResponse {
  case class Continue(exchange: HttpExchange) extends FilterResponse
  case class Stop(exchange: HttpExchange) extends FilterResponse
}