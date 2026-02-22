package spice.http.server.handler

import fabric.*
import fabric.rw.*
import rapid.{Task, logger}
import scribe.mdc.MDC
import spice.{ExceptionType, UserException}
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.net.ContentType

trait LifecycleHandler extends HttpHandler {
  protected def preHandle(exchange: HttpExchange): Task[HttpExchange]

  protected def apply(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange]

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
      val content = throwable2Content(exchange, throwable)
      response.withContent(content)
    }
  }

  protected def throwable2Content(exchange: HttpExchange, throwable: Throwable): Content =
    LifecycleHandler.throwable2Content(exchange, throwable, ContentType.`text/plain`)

  protected def notFoundContent: Task[Content] = Task.pure(LifecycleHandler.DefaultNotFound)

  override final def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = {
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

  def throwable2Content(exchange: HttpExchange, throwable: Throwable, contentType: ContentType): Content = throwable match {
    case exc: UserException =>
      exc.`type` match {
        case ExceptionType.Info => // Nothing needs to be logged
        case ExceptionType.Warn => scribe.warn(s"${exchange.request.url} failed with ${exc.fullMessage}")
        case ExceptionType.Error => scribe.error(s"${exchange.request.url} failed with ${exc.fullMessage}", exc)
      }
      errorMessageToContent(exc.message, exc.code, contentType)
    case _ => errorMessageToContent(s"An internal error occurred: ${throwable.getClass.getSimpleName}", None, contentType)
  }

  protected def errorMessageToContent(message: String,
                                      code: Option[Int],
                                      contentType: ContentType): Content = contentType match {
    case ContentType.`text/plain` => errorMessage2StringContent(message, code)
    case ContentType.`application/json` => errorMessage2JsonContent(message, code)
    case _ => throw new RuntimeException(s"Unsupported content type: $contentType")
  }

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

  protected def errorMessage2JsonContent(message: String, code: Option[Int]): Content = Content.json(obj(
    "error" -> obj(
      "message" -> message.json,
      "code" -> code.json
    )
  ))
}