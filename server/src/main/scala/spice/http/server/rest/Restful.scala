package spice.http.server.rest

import cats.effect.IO
import fabric.io.JsonParser
import fabric.rw._
import fabric.{Json, Obj, Str, arr, obj, str}
import profig.Profig
import scribe.mdc.MDC
import spice.http.content.FormDataEntry.FileEntry
import spice.{UserException, ValidationError}
import spice.http.content.{Content, FormDataContent}
import spice.http.server.dsl.{ConnectionFilter, FilterResponse, PathFilter}
import spice.http.{HttpExchange, HttpMethod, HttpStatus}
import spice.net.{URL, URLPath}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.language.experimental.macros

abstract class Restful[Request, Response](implicit val requestRW: RW[Request],
                                          val responseRW: RW[Response]) extends ConnectionFilter {
  protected def allowGet: Boolean = true

  def pathOption: Option[URLPath] = None

  def apply(exchange: HttpExchange, request: Request)(implicit mdc: MDC): IO[RestfulResponse[Response]]

  def validations: List[RestfulValidation[Request]] = Nil

  def error(throwable: Throwable): RestfulResponse[Response] = {
    scribe.error(throwable)
    error(List(ValidationError("An internal error occurred")), HttpStatus.InternalServerError)
  }

  def error(message: String): RestfulResponse[Response] =
    error(List(ValidationError(message)), HttpStatus.InternalServerError)

  def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Response]

  def timeout: Duration = Duration.Inf

  protected def ok(response: Response): RestfulResponse[Response] = this.response(response, HttpStatus.OK)

  protected def response(response: Response, status: HttpStatus): RestfulResponse[Response] = {
    RestfulResponse(response, status)
  }

  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[FilterResponse] = if (accept(exchange)) {
    handle(exchange).map { e =>
      FilterResponse.Continue(e)
    }
  } else {
    IO.pure(FilterResponse.Stop(exchange))
  }

  protected lazy val acceptedMethods: List[HttpMethod] =
    List(HttpMethod.Options, HttpMethod.Post) ::: (if (allowGet) List(HttpMethod.Get) else Nil)

  private def accept(exchange: HttpExchange): Boolean = {
    if (acceptedMethods.contains(exchange.request.method)) {
      pathOption match {
        case Some(p) => p == exchange.request.url.path
        case None => true
      }
    } else {
      false
    }
  }

  override def handle(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = if (accept(exchange)) {
    if (exchange.request.method == HttpMethod.Options) {
      exchange.modify { response =>
        IO {
          response
            .withStatus(HttpStatus.OK)
            .withHeader("Allow", acceptedMethods.mkString(", "))
            .withContent(Content.none)
        }
      }
    } else {
      // Build JSON
      val io: IO[RestfulResponse[Response]] = {
        Restful.jsonFromExchange(exchange).flatMap { json =>
          // Decode request
          val req = json.as[Request]
          // Validations
          Restful.validate[Request](req, validations) match {
            case Left(errors) =>
              val status = errors.map(_.status).max
              IO.pure(error(errors, status))
            case Right(request) => try {
              var updatedRequest = request match {
                case r: MultipartRequest => exchange.request.content match {
                  case Some(content: FormDataContent) => r.withContent(content).asInstanceOf[Request]
                  case _ => request
                }
                case _ => request
              }
              updatedRequest = updatedRequest match {
                case r: ExchangeRequest => r.withExchange(exchange).asInstanceOf[Request]
                case _ => updatedRequest
              }
              apply(exchange, updatedRequest)
                .timeout(timeout)
                .handleError { throwable =>
                  error(throwable)
                }
            } catch {
              case t: Throwable => IO.pure(error(t))
            }
          }
        }
      }

      io.flatMap { result =>
        responseToContent(result.response).flatMap { content =>
          exchange.modify { httpResponse =>
            IO.pure(httpResponse.withContent(content).withStatus(result.status))
          }
        }
      }
    }
  } else {
    IO.pure(exchange)
  }

  protected def responseToContent(response: Response): IO[Content] = response match {
    case content: Content => IO.pure(content)
    case _ => IO.blocking {
      val json = response.json
      Content.json(json, compact = false)
    }
  }
}

object Restful {
  private val key: String = "restful"

  def store(exchange: HttpExchange, json: Json): Unit = {
    val merged = Json.merge(exchange.store.getOrElse[Json](key, obj()), json)
    exchange.store.update[Json](key, merged)
  }

  def apply[Request: RW, Response: RW](handler: Request => IO[Response],
                                               path: Option[URLPath] = None): Restful[Request, Response] =
    new Restful[Request, Response] {
      override def pathOption: Option[URLPath] = path

      override def apply(exchange: HttpExchange, request: Request)
                        (implicit mdc: MDC): IO[RestfulResponse[Response]] = handler(request)
        .map { response =>
          ok(response)
        }

      override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Response] =
        throw UserException(s"Error occurred: ${errors.map(_.message).mkString(", ")}")
    }

  def validate[Request](request: Request,
                        validations: List[RestfulValidation[Request]]): Either[List[ValidationError], Request] = {
    val errors = ListBuffer.empty[ValidationError]
    var r: Request = request
    validations.foreach { v =>
      v.validate(r) match {
        case Left(err) => errors += err
        case Right(req) => r = req
      }
    }
    if (errors.nonEmpty) {
      Left(errors.toList)
    } else {
      Right(r)
    }
  }

  def jsonFromExchange(exchange: HttpExchange): IO[Json] = {
    val request = exchange.request
    val content = request.content match {
      case Some(content) => jsonFromContent(content)
      case None => IO.pure(obj())
    }
    content.map { contentJson =>
      val urlJson = jsonFromURL(request.url)
      val pathJson = PathFilter.argumentsFromConnection(exchange).json
      val storeJson = exchange.store.getOrElse[Json](key, obj())
      List(urlJson, pathJson, storeJson).foldLeft(contentJson)((merged, json) => {
        if (json.nonEmpty) {
          Json.merge(merged, json)
        } else {
          merged
        }
      })
    }
  }

  def jsonFromContent(content: Content): IO[Json] = content match {
    case fdc: FormDataContent => IO {
      val jsonValues = fdc.strings.map {
        case (key, entry) => try {
          key -> JsonParser(entry.value)
        } catch {
          case t: Throwable => throw new RuntimeException(s"Failed to convert key: $key, value: ${entry.value} to JSON (${entry.headers.map.map(t => s"${t._1} = ${t._2.mkString(",")}")})!", t)
        }
      }
      val fileValues = fdc.files.map {
        case (key, entry) => key -> obj().withReference(FileUpload(
          fileName = entry.fileName,
          file = entry.file,
          headers = entry.headers
        ))
      }
      Obj(jsonValues ++ fileValues)
    }
    case _ =>
      content.asString.map {
        case "" => obj()
        case contentString =>
          val firstChar = contentString.charAt(0)
          val json = if (Set('"', '{', '[').contains(firstChar)) {
            JsonParser(contentString)
          } else {
            Str(contentString)
          }
          json
      }
  }

  def jsonFromURL(url: URL): Json = {
    val p = Profig.empty
    url.parameters.map.toList.foreach {
      case (key, param) =>
        val values = param.values
        val valuesJson = if (values.length > 1) {
          arr(values.map(str): _*)
        } else {
          str(values.head)
        }
        p(key).merge(valuesJson)
    }
    p.json
  }
}