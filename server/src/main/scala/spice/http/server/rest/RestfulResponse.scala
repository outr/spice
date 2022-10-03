package spice.http.server.rest

import spice.http.HttpStatus

case class RestfulResponse[Response](response: Response, status: HttpStatus)
