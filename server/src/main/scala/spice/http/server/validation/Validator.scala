package spice.http.server.validation

import cats.effect.IO
import spice.http.HttpExchange

trait Validator {
  def validate(exchange: HttpExchange): IO[ValidationResult]
}