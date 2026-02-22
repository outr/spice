package spice.http.server

import fabric.rw.*
import rapid.Task
import scribe.mdc.MDC
import spice.http.content.Content
import spice.http.server.handler.*
import spice.http.server.rest.Restful
import spice.http.server.validation.{ValidationResult, Validator}
import spice.http.{HttpExchange, HttpMethod, HttpStatus}
import spice.net.{ContentType, IP, URLMatcher, URLPath}

import scala.language.implicitConversions

package object dsl {
  private[spice] val DeltaKey: String = "deltas"

  given Conversion[Validator, ConnectionFilter] = validator => new ConnectionFilter {
    private lazy val list = List(validator)

    override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
      ValidatorHttpHandler.validate(exchange, list).map {
        case ValidationResult.Continue(c) => FilterResponse.Continue(c)
        case vr => FilterResponse.Stop(vr.exchange)
      }
    }
  }

  given Conversion[HttpMethod, ConnectionFilter] = method => new ConditionalFilter(_.request.method == method)

  given handler2Filter: Conversion[HttpHandler, ConnectionFilter] = handler => ActionFilter { exchange =>
    if (PathPart.fulfilled(exchange)) {
      handler.handle(exchange)
    } else {
      Task.pure(exchange)
    }
  }

  given Conversion[CachingManager, ConnectionFilter] = caching => new LastConnectionFilter(handler2Filter(caching))

  given Conversion[String, ConnectionFilter] = s => new ConnectionFilter {
    override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = Task {
      val path = if (s.startsWith("/")) {
        s
      } else {
        s"/$s"
      }
      PathPart.take(exchange, URLPath.parse(path)) match {
        case Some(c) => FilterResponse.Continue(c)
        case None => FilterResponse.Stop(exchange)
      }
    }
  }

  given Conversion[URLMatcher, ConnectionFilter] = matcher => new ConditionalFilter(c => matcher.matches(c.request.url))

  given content2Filter: Conversion[Content, ConnectionFilter] = content => handler2Filter(ContentHandler(content, HttpStatus.OK))

  given path2AllowFilter: Conversion[URLPath, ConnectionFilter] = path => PathFilter(path)

  given connectionFilters2ConnectionFilter: Conversion[List[ConnectionFilter], ConnectionFilter] = list =>
    ListConnectionFilter(list.sorted)

  def filters(filters: ConnectionFilter*): ConnectionFilter = ListConnectionFilter(filters.toList)

  def allow(ips: IP*): ConnectionFilter = IPAddressFilter(allow = ips.toList)

  def allow(path: URLPath): ConnectionFilter = PathFilter(path)

  def last(filters: ConnectionFilter*): ConnectionFilter = LastConnectionFilter(filters*)

  def respond(content: Content, status: HttpStatus = HttpStatus.OK): ContentHandler = {
    ContentHandler(content, status)
  }

  def redirect(path: URLPath): ConnectionFilter = new ConnectionFilter {
    override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
      HttpHandler.redirect(exchange, path.encoded).map { redirected =>
        FilterResponse.Continue(redirected)
      }
    }
  }

  given string2Content: Conversion[String, Content] = value => Content.string(value, ContentType.`text/plain`)
}
