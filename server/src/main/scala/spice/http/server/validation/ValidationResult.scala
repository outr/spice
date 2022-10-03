package spice.http.server.validation

import spice.http.HttpExchange

sealed trait ValidationResult {
  def exchange: HttpExchange
}

object ValidationResult {
  case class Continue(exchange: HttpExchange) extends ValidationResult
  case class Redirect(exchange: HttpExchange, location: String) extends ValidationResult
  case class Error(exchange: HttpExchange, status: Int, message: String) extends ValidationResult
}