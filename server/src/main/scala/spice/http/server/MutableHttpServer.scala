package spice.http.server

import rapid.Task
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

  override final def apply(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = handleInternal(exchange)

  protected def handleInternal(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = {
    handleRecursive(exchange, handlers()).flatMap { updated =>
      // NotFound handling
      if (updated.response.content.isEmpty && updated.response.status == HttpStatus.OK) {
        updated.modify { response =>
          Task(response.copy(status = HttpStatus.NotFound))
        }.flatMap { notFound =>
          errorHandler.get.handle(notFound, None)
        }
      } else {
        Task.pure(updated)
      }
    }
  }

  private def handleRecursive(exchange: HttpExchange, handlers: List[HttpHandler])
                             (using mdc: MDC): Task[HttpExchange] = {
    if (exchange.finished || handlers.isEmpty) {
      Task.pure(exchange) // Finished
    } else {
      val handler = handlers.head
      val startMs = System.currentTimeMillis()
      handler.handle(exchange).flatMap { updated =>
        val elapsed = System.currentTimeMillis() - startMs
        if (elapsed > MutableHttpServer.SlowHandlerMs) {
          scribe.warn(s"Slow handler: ${handler.getClass.getName} took ${elapsed}ms for ${exchange.request.url.path}")
        }
        handleRecursive(updated, handlers.tail)
      }
    }
  }
}

object MutableHttpServer {
  /**
   * How long a single handler may take before it is worth a warning.
   *
   * This was 100ms, which is under the cost of ordinary work: serving a cached image, a paged query,
   * anything touching a database. On one production server it produced 781 warnings in a day, the
   * largest single source, for handlers doing exactly what they are supposed to - the example that
   * finally prompted this was a provider logo taking 118ms.
   *
   * A threshold that fires on normal behaviour does not report slowness, it reports existence, and it
   * trains everyone reading the log to skip the category. One second is genuinely slow for an HTTP
   * handler and matches what applications typically consider a slow call.
   *
   * Overridable so an application with tighter expectations can lower it deliberately, rather than
   * inheriting a number that suits nobody.
   */
  var SlowHandlerMs: Long = 1000L
}
