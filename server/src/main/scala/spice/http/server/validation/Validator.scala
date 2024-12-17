package spice.http.server.validation

import rapid.Task
import spice.http.HttpExchange

trait Validator {
  def validate(exchange: HttpExchange): Task[ValidationResult]
}