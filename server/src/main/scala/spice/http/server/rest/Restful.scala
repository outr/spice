package spice.http.server.rest

import cats.effect.IO
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw._
import fabric.{Json, Str, arr, obj, str}
import spice.ValidationError
import spice.http.content.Content
import spice.http.server.dsl.{ConnectionFilter, FilterResponse, PathFilter}
import spice.http.{HttpExchange, HttpMethod, HttpStatus}
import spice.net.{ContentType, URL, URLPath}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.language.experimental.macros

abstract class Restful[Request, Response](implicit val requestRW: RW[Request],
                                          val responseRW: RW[Response]) extends ConnectionFilter {
  protected def allowGet: Boolean = true

  def pathOption: Option[URLPath] = None

  def apply(exchange: HttpExchange, request: Request): IO[RestfulResponse[Response]]

  def validations: List[RestfulValidation[Request]] = Nil

  def error(message: String): RestfulResponse[Response] =
    error(List(ValidationError(message)), HttpStatus.InternalServerError)

  def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Response]

  def timeout: Duration = Duration.Inf

  protected def ok(response: Response): RestfulResponse[Response] = this.response(response, HttpStatus.OK)

  protected def response(response: Response, status: HttpStatus): RestfulResponse[Response] = {
    RestfulResponse(response, status)
  }

  override def apply(exchange: HttpExchange): IO[FilterResponse] = if (accept(exchange)) {
    handle(exchange).map { e =>
      FilterResponse.Continue(e)
    }
  } else {
    IO.pure(FilterResponse.Stop(exchange))
  }

  private lazy val acceptedMethods = List("OPTIONS", "POST") ::: (if (allowGet) List("GET") else Nil)

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

  override def handle(exchange: HttpExchange): IO[HttpExchange] = if (accept(exchange)) {
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
        val json = Restful.jsonFromExchange(exchange)
        // Decode request
        val req = json.as[Request]
        // Validations
        Restful.validate(req, validations) match {
          case Left(errors) => {
            val status = errors.map(_.status).max
            IO.pure(error(errors, status))
          }
          case Right(request) => try {
            apply(exchange, request)
              .timeout(timeout)
              .handleError { throwable =>
                scribe.error(s"Error occurred in ${getClass.getName}", throwable)
                error("An internal error occurred.")
              }
          } catch {
            case t: Throwable => {
              val err = ValidationError(s"Error while calling restful: ${t.getMessage}", ValidationError.Internal)
              IO.pure(error(List(err), err.status))
            }
          }
        }
      }

      io.flatMap { result =>
        // Encode response
        val responseJsonString = JsonFormatter.Default(result.response.json)

        // Attach content
        exchange.modify { httpResponse =>
          IO {
            httpResponse
              .withContent(Content.string(responseJsonString, ContentType.`application/json`))
              .withStatus(result.status)
          }
        }
      }
    }
  } else {
    IO.pure(exchange)
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

      override def apply(exchange: HttpExchange, request: Request): IO[RestfulResponse[Response]] = handler(request)
        .map { response =>
          ok(response)
        }

      override def error(errors: List[ValidationError], status: HttpStatus): RestfulResponse[Response] =
        throw new RuntimeException(s"Error occurred: ${errors.map(_.message).mkString(", ")}")
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

  def jsonFromExchange(exchange: HttpExchange): Json = {
    val request = exchange.request
    val contentJson = request.content.map(jsonFromContent).flatMap(_.toOption).getOrElse(obj())
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

  def jsonFromContent(content: Content): Either[ValidationError, Json] = {
    val contentString = content.asString
    val firstChar = contentString.charAt(0)
    val json = if (Set('"', '{', '[').contains(firstChar)) {
      JsonParser(contentString)
    } else {
      Str(contentString)
    }
    Right(json)
  }

  def jsonFromURL(url: URL): Json = {
    val entries = url.parameters.map.toList.map {
      case (key, param) =>
        val values = param.values
        val valuesJson = if (values.length > 1) {
          arr(values.map(str): _*)
        } else {
          str(values.head)
        }
        key -> valuesJson
    }
    obj(entries: _*)
  }
}