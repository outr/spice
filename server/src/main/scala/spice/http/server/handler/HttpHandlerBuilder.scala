package spice.http.server.handler

import cats.effect.IO
import fabric._
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw._
import scribe.Priority
import scribe.mdc.MDC
import spice.http.content.{Content, StringContent}
import spice.http.server.dsl.ClassLoaderPath
import spice.http.{HttpExchange, HttpMethod, HttpRequest}
import spice.http.server.MutableHttpServer
import spice.http.server.validation.{ValidationResult, Validator}
import spice.net
import spice.net.{ContentType, URL, URLMatcher}

import java.io.File
import scala.util.Try

case class HttpHandlerBuilder(server: MutableHttpServer,
                              urlMatcher: Option[URLMatcher] = None,
                              requestMatchers: Set[HttpRequest => Boolean] = Set.empty,
                              cachingManager: CachingManager = CachingManager.Default,
                              priority: Priority = Priority.Normal,
                              validators: List[Validator] = Nil) {
  def priority(priority: Priority): HttpHandlerBuilder = copy(priority = priority)

  def matcher(urlMatcher: URLMatcher): HttpHandlerBuilder = copy(urlMatcher = Some(urlMatcher))

  def requestMatcher(requestMatcher: HttpRequest => Boolean): HttpHandlerBuilder = copy(requestMatchers = requestMatchers + requestMatcher)

  def methodMatcher(method: HttpMethod): HttpHandlerBuilder = requestMatcher(request => request.method == method)

  def caching(cachingManager: CachingManager): HttpHandlerBuilder = copy(cachingManager = cachingManager)

  def withValidation(validators: Validator*): HttpHandlerBuilder = copy(validators = validators.toList ::: this.validators)

  def resource(f: => Content): HttpHandler = resource((_: URL) => Some(f))

  def resource(f: URL => Option[Content]): HttpHandler = {
    handle { exchange =>
      f(exchange.request.url).map { content =>
        SenderHandler(content, caching = cachingManager).handle(exchange)
      }.getOrElse(IO.pure(exchange))
    }
  }

  def file(directory: File, pathTransform: String => String = (s: String) => s): HttpHandler = {
    handle { exchange =>
      val path = pathTransform(exchange.request.url.path.decoded)
      val file = new File(directory, path)
      if (file.isFile) {
        SenderHandler(Content.file(file), caching = cachingManager).handle(exchange)
      } else {
        IO.pure(exchange)
      }
    }
  }

  def classLoader(directory: String = "", pathTransform: String => String = (s: String) => s): HttpHandler =
    ClassLoaderPath(directory, pathTransform)

  /*def stream(baseDirectory: File, basePath: String, deltas: HttpExchange => List[Delta] = _ => Nil): HttpHandler = handle { exchange =>
    val url = exchange.request.url
    val path = url.path.decoded
    if (path.startsWith(basePath)) {
      val clippedPath = path.substring(basePath.length)
      val file = new File(baseDirectory, clippedPath)
      if (file.exists()) {
        val parser = HTMLParser.cache(file)
        val selector = url.param("selector").map(Selector.parse)
        val mods = deltas(exchange)
        val html = parser.stream(mods, selector)
        val content = StringContent(html, ContentType.`text/html`, file.lastModified())
        val handler = SenderHandler(content, caching = cachingManager)
        handler.handle(exchange)
      } else {
        IO.pure(exchange)
      }
    } else {
      IO.pure(exchange)
    }
  }*/

  def handle(f: HttpExchange => IO[HttpExchange]): HttpHandler = wrap(new HttpHandler {
    override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = f(exchange)
  })

  def validation(validator: HttpExchange => IO[ValidationResult]): HttpHandler = validation(new Validator {
    override def validate(exchange: HttpExchange): IO[ValidationResult] = validator(exchange)
  })

  def validation(validators: Validator*): HttpHandler = wrap(new ValidatorHttpHandler(validators.toList))

  def redirect(path: net.URLPath): HttpHandler = handle { exchange =>
    HttpHandler.redirect(exchange, path.encoded)
  }

  def content(content: => Content): HttpHandler = handle { exchange =>
    exchange.modify { response =>
      IO(response.withContent(content))
    }
  }

  def restful[Request, Response](handler: Request => Response)
                                (implicit requestRW: RW[Request], responseRW: RW[Response]): HttpHandler = {
    handle { exchange =>
      val jsonOption: Option[Json] = exchange.request.method match {
        case HttpMethod.Get => {
          Some(obj(exchange.request.url.parameters.entries.map {
            case (key, param) => key -> str(param.value)
          }: _*))
        }
        case _ => exchange.request.content match {
          case Some(content) => content match {
            case StringContent(jsonString, _, _) => Try(JsonParser(jsonString)).toOption
            case _ =>
              scribe.error(s"Unsupported content for restful end-point: $content.")
              None
          }
          case None => None     // Ignore calls to this end-point that have no content
        }
      }
      jsonOption.map { json =>
        val request: Request = json.as[Request]
        val response: Response = handler(request)
        val responseJsonString = JsonFormatter.Default(response.json)
        exchange.modify { httpResponse =>
          IO(httpResponse.withContent(Content.string(responseJsonString, ContentType.`application/json`)))
        }
      }.getOrElse(IO.pure(exchange))
    }
  }

//  def zip(entries: ZipFileEntry*): HttpHandler = content(new StreamZipContent(entries.toList))

  def wrap(handler: HttpHandler): HttpHandler = {
    val p = if (priority == Priority.Normal) handler.priority else priority
    val wrapper = new HttpHandler {
      override def priority: Priority = p

      override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = {
        if (urlMatcher.forall(_.matches(exchange.request.url)) && requestMatchers.forall(_(exchange.request))) {
          ValidatorHttpHandler.validate(exchange, validators).flatMap {
            case ValidationResult.Continue(c) => handler.handle(c)
            case vr => IO.pure(vr.exchange) // Validation failed, handled by ValidatorHttpHandler
          }
        } else {
          IO.pure(exchange)
        }
      }
    }
    server.handlers += wrapper
    handler
  }

  def apply(handler: HttpHandler): HttpHandler = wrap(handler)
}