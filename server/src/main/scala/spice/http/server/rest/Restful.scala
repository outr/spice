package spice.http.server.rest

import cats.effect.IO
import fabric.rw._
import spice.ValidationError
import spice.http.server.handler.HttpHandler
import spice.http.{HttpExchange, HttpStatus}
import spice.net.URLPath

import scala.concurrent.duration.Duration
import scala.language.experimental.macros

trait Restful[Request, Response] {
  def pathOption: Option[URLPath] = None

  def apply(exchange: HttpExchange, request: Request): IO[RestfulResponse[Response]]

  def validations: List[RestfulValidation[Request]] = Nil

  def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Response]

  def timeout: Duration = Duration.Inf

  protected def ok(response: Response): RestfulResponse[Response] = this.response(response, HttpStatus.OK)

  protected def response(response: Response, status: HttpStatus): RestfulResponse[Response] = {
    RestfulResponse(response, status)
  }
}

object Restful {
  def apply[Request, Response](handler: Request => IO[Response],
                               path: Option[URLPath] = None): Restful[Request, Response] = new Restful[Request, Response] {
    override def pathOption: Option[URLPath] = path

    override def apply(exchange: HttpExchange, request: Request): IO[RestfulResponse[Response]] = handler(request)
      .map { response =>
        ok(response)
      }

    override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Response] =
      throw new RuntimeException(s"Error occurred: ${errors.map(_.message).mkString(", ")}")
  }

  def handler[Request, Response](restful: Restful[Request, Response])
                              (implicit writer: Writer[Request], reader: Reader[Response]): RestfulHandler[Request, Response] = {
    new RestfulHandler[Request, Response](restful)(writer, reader)
  }
}