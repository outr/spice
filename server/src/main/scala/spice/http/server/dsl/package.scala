package spice.http.server

import cats.effect.IO
import fabric.rw._
import spice.delta.types.Delta
import spice.http.content.Content
import spice.http.{HttpExchange, HttpMethod, HttpStatus}
import spice.http.server.handler.{CachingManager, ContentHandler, HttpHandler, SenderHandler, ValidatorHttpHandler}
import spice.http.server.rest.Restful
import spice.http.server.validation.{ValidationResult, Validator}
import spice.net.{ContentType, IP, Path, URLMatcher}

import java.io.File
import scala.language.implicitConversions

package object dsl {
  private[spice] val DeltaKey: String = "deltas"

  implicit class ValidatorFilter(val validator: Validator) extends ConnectionFilter {
    private lazy val list = List(validator)

    override def apply(exchange: HttpExchange): IO[FilterResponse] = {
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
      IO.pure(exchange)
    }
  }

  implicit class CachingManagerFilter(val caching: CachingManager) extends LastConnectionFilter(handler2Filter(caching))

//  implicit class DeltasFilter(val deltas: List[Delta]) extends ActionFilter(exchange => IO {
//    exchange.deltas ++= deltas
//    exchange
//  })
//
//  implicit class DeltaFilter(delta: Delta) extends ActionFilter(exchange => IO {
//    exchange.deltas += delta
//    exchange
//  })

  implicit class StringFilter(val s: String) extends ConnectionFilter {
    override def apply(exchange: HttpExchange): IO[FilterResponse] = IO {
      PathPart.take(exchange, s) match {
        case Some(c) => FilterResponse.Continue(c)
        case None => FilterResponse.Stop(exchange)
      }
    }
  }

  implicit class URLMatcherFilter(val matcher: URLMatcher) extends ConditionalFilter(c => matcher.matches(c.request.url))

  case class ClassLoaderPath(directory: String = "", pathTransform: String => String = (s: String) => s) extends ConnectionFilter {
    private val dir = if (directory.endsWith("/")) {
      directory.substring(directory.length - 1)
    } else {
      directory
    }

    override def apply(exchange: HttpExchange): IO[FilterResponse] = {
      val path = pathTransform(exchange.request.url.path.decoded)
      val resourcePath = s"$dir$path" match {
        case s if s.startsWith("/") => s.substring(1)
        case s => s
      }
      Option(getClass.getClassLoader.getResource(resourcePath))
        .map(url => Content.url(url))
        .map(content => SenderHandler(content).handle(exchange))
        .map(_.map(FilterResponse.Continue.apply))
        .getOrElse(IO.pure(FilterResponse.Stop(exchange)))
    }
  }

  implicit def content2Filter(content: Content): ConnectionFilter = handler2Filter(ContentHandler(content, HttpStatus.OK))

  implicit def restful[Request, Response](restful: Restful[Request, Response])
                                         (implicit writer: Writer[Request], reader: Reader[Response]): ConnectionFilter = {
    handler2Filter(Restful(restful)(writer, reader))
  }

  implicit def path2AllowFilter(path: Path): ConnectionFilter = PathFilter(path)

  def filters(filters: ConnectionFilter*): ConnectionFilter = ListConnectionFilter(filters.toList)

  def allow(ips: IP*): ConnectionFilter = IPAddressFilter(allow = ips.toList)

  def allow(path: Path): ConnectionFilter = PathFilter(path)

  def last(filters: ConnectionFilter*): ConnectionFilter = LastConnectionFilter(filters: _*)

  def respond(content: Content, status: HttpStatus = HttpStatus.OK): ContentHandler = {
    ContentHandler(content, status)
  }

  def redirect(path: Path): ConnectionFilter = new ConnectionFilter {
    override def apply(exchange: HttpExchange): IO[FilterResponse] = {
      HttpHandler.redirect(exchange, path.encoded).map { redirected =>
        FilterResponse.Continue(redirected)
      }
    }
  }

  implicit def string2Content(value: String): Content = Content.string(value, ContentType.`text/plain`)
}