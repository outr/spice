package spice.http.server

import cats.effect.IO
import reactify.Var
import scribe.mdc.MDC
import spice.ItemContainer
import spice.http.{HttpExchange, HttpStatus}
import spice.http.server.handler.{HttpHandler, HttpHandlerBuilder}

class MutableHttpServer extends HttpServer {
  val handler: HttpHandlerBuilder = HttpHandlerBuilder(this)

  /**
   * The error handler if an error is thrown. This is used automatically when an HttpHandler fires a Throwable but
   * can be explicitly used for more specific errors. The error handler is responsible for applying an existing
   * status on the HttpResponse or setting one if the status is a non-error.
   *
   * Defaults to DefaultErrorHandler
   */
  val errorHandler: Var[ErrorHandler] = Var(DefaultErrorHandler)

  object handlers extends ItemContainer[HttpHandler]

  override final def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = handleInternal(exchange)

  protected def handleInternal(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = {
    handleRecursive(exchange, handlers()).flatMap { updated =>
      // NotFound handling
      if (updated.response.content.isEmpty && updated.response.status == HttpStatus.OK) {
        updated.modify { response =>
          IO(response.copy(status = HttpStatus.NotFound))
        }.flatMap { notFound =>
          errorHandler.get.handle(notFound, None)
        }
      } else {
        IO.pure(updated)
      }
    }
  }

  private def handleRecursive(exchange: HttpExchange, handlers: List[HttpHandler])
                             (implicit mdc: MDC): IO[HttpExchange] = {
    if (exchange.finished || handlers.isEmpty) {
      IO.pure(exchange) // Finished
    } else {
      val handler = handlers.head
      handler.handle(exchange).flatMap { updated =>
        handleRecursive(updated, handlers.tail)
      }
    }
  }
}
