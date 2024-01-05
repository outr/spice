package spice.http.server.rest

import spice.http.HttpExchange

/**
 * Drop-in convenience class to wrap around an existing Request object and give a reference to the `HttpExchange`.
 */
trait ExchangeRequest {
  def exchange: HttpExchange

  def withExchange(exchange: HttpExchange): ExchangeRequest
}