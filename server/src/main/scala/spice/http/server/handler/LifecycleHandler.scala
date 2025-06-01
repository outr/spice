package spice.http.server.handler

import fabric._
import fabric.rw._
import rapid.{Task, logger}
import scribe.mdc.MDC
import spice.UserException
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.net.ContentType

trait LifecycleHandler extends HttpHandler {
  protected def preHandle(exchange: HttpExchange): Task[HttpExchange]

  protected def apply(exchange: HttpExchange)(implicit mdc: MDC): Task[HttpExchange]

  protected def postHandle(exchange: HttpExchange): Task[HttpExchange]

  protected def errorHandler(exchange: HttpExchange,
                             state: LifecycleState,
                             throwable: Throwable): Task[HttpExchange] = for {
    _ <- errorLogger(throwable, Some(exchange), Some(state))
    modified <- errorResponse(exchange, state, throwable)
  } yield {
    modified
  }

  def errorLogger(throwable: Throwable,
                            exchange: Option[HttpExchange],
                            state: Option[LifecycleState]): Task[Unit] =
    logger.error(s"Error occurred while handling ${exchange.map(_.request.url)} ($state): ", throwable)

  protected def errorResponse(exchange: HttpExchange,
                              state: LifecycleState,
                              throwable: Throwable): Task[HttpExchange] = exchange.modify { response =>
    Task {
      val content = throwable match {
        case UserException(message, code) => errorMessageToContent(message, code)
        case _ => errorMessageToContent(s"An internal error occurred: ${throwable.getClass.getSimpleName}", None)
      }
      response.withContent(content)
    }
  }

  protected def errorMessageToContent(message: String, code: Option[Int]): Content =
    errorMessage2StringContent(message, code)

  protected def errorMessage2StringContent(message: String, code: Option[Int]): Content = {
    val output = code match {
      case Some(c) => s"$message ($c)"
      case None => message
    }
    Content.string(
      value = output,
      contentType = ContentType.`text/plain`
    )
  }

  protected def errorMessage2JsonContent(message: String, code: Option[Int]): Content = {
    Content.json(obj(
      "message" -> str(message),
      "code" -> code.json
    ))
  }

  protected def notFoundContent: Task[Content] = Task.pure(LifecycleHandler.DefaultNotFound)

  override final def handle(exchange: HttpExchange)(implicit mdc: MDC): Task[HttpExchange] = {
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
        Task.pure(e)
      }
    }.handleError { throwable =>
      errorHandler(currentExchange, state, throwable)
    }
  }
}

object LifecycleHandler {
  lazy val DefaultNotFound: Content = Content.string("Not found!", ContentType.`text/plain`)
}