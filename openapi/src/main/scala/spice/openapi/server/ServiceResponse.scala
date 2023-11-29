package spice.openapi.server

import spice.http.HttpExchange

case class ServiceResponse[Response](exchange: HttpExchange)
