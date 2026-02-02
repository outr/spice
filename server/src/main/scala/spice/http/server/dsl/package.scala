package spice.http.server

import fabric.rw._
import rapid.Task
import scribe.mdc.MDC
import spice.http.content.Content
import spice.http.server.handler._
import spice.http.server.rest.Restful
import spice.http.server.validation.{ValidationResult, Validator}
import spice.http.{HttpExchange, HttpMethod, HttpStatus}
import spice.net.{ContentType, IP, URLMatcher, URLPath}

import scala.language.implicitConversions

package object dsl {
  private[spice] val DeltaKey: String = "deltas"

  implicit class ValidatorFilter(val validator: Validator) extends ConnectionFilter {
    private lazy val list = List(validator)

    override def apply(exchange: HttpExchange)(implicit mdc: MDC): Task[FilterResponse] = {
      ValidatorHttpHandler.validate(exchange, list).map {
        case ValidationResult.Continue(c) => FilterResponse.Continue(c)
        case vr => FilterResponse.Stop(vr.exchange)
      }
    }
  }

  implicit class MethodConnectionFilter(val method: HttpMethod) extends ConditionalFilter(_.request.method == method)

  implicit def handler2Filter(handler: HttpHandler): ConnectionFilter = ActionFilter { exchange =>
    if (PathPart.fulfilled(exchange)) {
      handler.handle(exchange)
    } else {
      Task.pure(exchange)
    }
  }

  implicit class CachingManagerFilter(val caching: CachingManager) extends LastConnectionFilter(handler2Filter(caching))

//  implicit class DeltasFilter(val deltas: List[Delta]) extends ActionFilter(exchange => Task {
//    exchange.deltas ++= deltas
//    exchange
//  })
//
//  implicit class DeltaFilter(delta: Delta) extends ActionFilter(exchange => Task {
//    exchange.deltas += delta
//    exchange
//  })

  implicit class StringFilter(val s: String) extends ConnectionFilter {
    override def apply(exchange: HttpExchange)(implicit mdc: MDC): Task[FilterResponse] = Task {
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

  implicit class URLMatcherFilter(val matcher: URLMatcher) extends ConditionalFilter(c => matcher.matches(c.request.url))

  implicit def content2Filter(content: Content): ConnectionFilter = handler2Filter(ContentHandler(content, HttpStatus.OK))

  implicit def path2AllowFilter(path: URLPath): ConnectionFilter = PathFilter(path)

  implicit def connectionFilters2ConnectionFilter(list: List[ConnectionFilter]): ConnectionFilter =
    ListConnectionFilter(list.sorted)

  def filters(filters: ConnectionFilter*): ConnectionFilter = ListConnectionFilter(filters.toList)

  def allow(ips: IP*): ConnectionFilter = IPAddressFilter(allow = ips.toList)

  def allow(path: URLPath): ConnectionFilter = PathFilter(path)

  def last(filters: ConnectionFilter*): ConnectionFilter = LastConnectionFilter(filters*)

  def respond(content: Content, status: HttpStatus = HttpStatus.OK): ContentHandler = {
    ContentHandler(content, status)
  }

  def redirect(path: URLPath): ConnectionFilter = new ConnectionFilter {
    override def apply(exchange: HttpExchange)(implicit mdc: MDC): Task[FilterResponse] = {
      HttpHandler.redirect(exchange, path.encoded).map { redirected =>
        FilterResponse.Continue(redirected)
      }
    }
  }

  implicit def string2Content(value: String): Content = Content.string(value, ContentType.`text/plain`)
}