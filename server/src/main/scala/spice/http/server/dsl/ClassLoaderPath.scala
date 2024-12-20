package spice.http.server.dsl

import rapid.Task
import scribe.mdc.MDC
import spice.http.HttpExchange
import spice.http.content.Content
import spice.http.server.handler.SenderHandler

case class ClassLoaderPath(classPathRoot: String = "",
                           pathTransform: String => String = (s: String) => s,
                           replaceExistingContent: Boolean = false) extends ConnectionFilter {
  private val dir = if (classPathRoot.endsWith("/")) {
    classPathRoot.substring(classPathRoot.length - 1)
  } else {
    classPathRoot
  }

  override def apply(exchange: HttpExchange)
                    (implicit mdc: MDC): Task[FilterResponse] = if (exchange.response.content.isEmpty || replaceExistingContent) {
    val path = pathTransform(exchange.request.url.path.decoded)
    val resourcePath = s"$dir$path" match {
      case s if s.startsWith("/") => s.substring(1)
      case s => s
    }
    Option(getClass.getClassLoader.getResource(resourcePath))
      .map(url => Content.url(url))
      .map(content => SenderHandler(content, replace = true).handle(exchange))
      .map { task =>
        task.map[FilterResponse] { exchange =>
          FilterResponse.Continue(exchange)
        }
      }
      .getOrElse(Task.pure(FilterResponse.Stop(exchange)))
  } else {
    Task.pure(FilterResponse.Stop(exchange))
  }
}