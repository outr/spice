package spice.http.server.handler

import cats.effect.IO
import scribe.cats.{io => logger}
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.net.ContentType

trait LifecycleHandler extends HttpHandler {
  protected def preHandle(exchange: HttpExchange): IO[HttpExchange]

  protected def apply(exchange: HttpExchange): IO[HttpExchange]

  protected def postHandle(exchange: HttpExchange): IO[HttpExchange]

  protected def errorHandler(exchange: HttpExchange,
                             state: LifecycleState,
                             throwable: Throwable): IO[HttpExchange] = for {
    _ <- errorLogger(throwable, Some(exchange), Some(state))
    modified <- errorResponse(exchange, state, throwable)
  } yield {
    modified
  }

  def errorLogger(throwable: Throwable,
                            exchange: Option[HttpExchange],
                            state: Option[LifecycleState]): IO[Unit] =
    logger.error(s"Error occurred while handling ${exchange.map(_.request.url)} ($state): ", throwable)

  protected def errorResponse(exchange: HttpExchange,
                              state: LifecycleState,
                              throwable: Throwable): IO[HttpExchange] = exchange.modify { response =>
    IO(response.withContent(Content.string(
      value = s"An internal error occurred: ${throwable.getClass.getSimpleName}",
      contentType = ContentType.`text/plain`
    )))
  }

  protected def notFoundContent: IO[Content] = IO.pure(LifecycleHandler.DefaultNotFound)

  override final def handle(exchange: HttpExchange): IO[HttpExchange] = {
    var currentExchange = exchange
    var state: LifecycleState = LifecycleState.Pre
    preHandle(exchange).flatMap { e =>
      currentExchange = e
      state = LifecycleState.Handler
      apply(e)
    }.flatMap { e =>
      currentExchange = e
      state = LifecycleState.Post
      postHandle(e)
    }.flatMap { e =>
      if (!e.finished && e.response.content.isEmpty) {
        e.modify { response =>
          notFoundContent.map { content =>
            response.withStatus(HttpStatus.NotFound).withContent(content)
          }
        }
      } else {
        IO.pure(e)
      }
    }.handleErrorWith { throwable =>
      errorHandler(currentExchange, state, throwable)
    }
  }
}

object LifecycleHandler {
  lazy val DefaultNotFound: Content = Content.string("Not found!", ContentType.`text/plain`)
}