package spice.http.server.handler

import rapid.Task
import scribe.mdc.MDC
import spice.http.content.Content
import spice.http.{HttpExchange, HttpStatus}
import spice.http.server.validation.ValidationResult.{Continue, Redirect}
import spice.http.server.validation.{ValidationResult, Validator}

class ValidatorHttpHandler(validators: List[Validator]) extends HttpHandler {
  override def handle(exchange: HttpExchange)(implicit mdc: MDC): Task[HttpExchange] = {
    ValidatorHttpHandler.validate(exchange, validators).map(_.exchange)
  }
}

object ValidatorHttpHandler {
  def validate(exchange: HttpExchange,
               validators: List[Validator]): Task[ValidationResult] = if (validators.isEmpty) {
    Task.pure(ValidationResult.Continue(exchange))
  } else {
    val validator = validators.head
    validator.validate(exchange).flatMap {
      case ValidationResult.Continue(c) => validate(c, validators.tail)
      case v: ValidationResult.Redirect =>
        HttpHandler.redirect(v.exchange, v.location).map(v.copy(_))
      case v: ValidationResult.Error => exchange.modify { response =>
        Task  (response.withStatus(HttpStatus(v.status, v.message)).withContent(Content.empty))
      }.map(_.finish()).map { exchange =>
        v.copy(exchange)
      }
    }
  }
}