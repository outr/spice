package spice.http.server.middleware

import rapid.Task
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpStatus}
import spice.http.server.dsl.{ConnectionFilter, FilterResponse}

trait MetricsFilter extends ConnectionFilter {
  def onRequestStart(exchange: HttpExchange): Task[Unit]
  def onRequestComplete(exchange: HttpExchange, durationMs: Long, status: HttpStatus): Task[Unit]
  def onRequestError(exchange: HttpExchange, throwable: Throwable): Task[Unit]

  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
    val startTime = System.currentTimeMillis()
    onRequestStart(exchange).map { _ =>
      exchange.store(MetricsFilter.StartTimeKey) = startTime
      continue(exchange)
    }
  }

  override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = {
    val startTime = System.currentTimeMillis()
    onRequestStart(exchange).flatMap { _ =>
      super.handle(exchange).flatMap { result =>
        val duration = System.currentTimeMillis() - startTime
        onRequestComplete(result, duration, result.response.status).map(_ => result)
      }.handleError { throwable =>
        onRequestError(exchange, throwable).flatMap { _ =>
          Task.error(throwable)
        }
      }
    }
  }
}

object MetricsFilter {
  val StartTimeKey: String = "metrics.startTime"
}
