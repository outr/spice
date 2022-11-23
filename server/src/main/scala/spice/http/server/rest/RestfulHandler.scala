package spice.http.server.rest

import cats.effect.IO
import fabric._
import fabric.io.{JsonFormatter, JsonParser}
import fabric.rw._
import spice.ValidationError
import spice.http.HttpExchange
import spice.http.content.Content
import spice.http.server.dsl.PathFilter
import spice.http.server.handler.HttpHandler
import spice.net.{ContentType, URL}

import scala.collection.mutable.ListBuffer

class RestfulHandler[Request, Response](restful: Restful[Request, Response])
                                       (implicit writer: Writer[Request], reader: Reader[Response]) extends HttpHandler {
  override def handle(exchange: HttpExchange): IO[HttpExchange] = {
    // Build JSON
    val io: IO[RestfulResponse[Response]] = {
      val json = RestfulHandler.jsonFromExchange(exchange)
      // Decode request
      val req = json.as[Request]
      // Validations
      RestfulHandler.validate(req, restful.validations) match {
        case Left(errors) => {
          val status = errors.map(_.status).max
          IO.pure(restful.error(errors, status))
        }
        case Right(request) => try {
          restful(exchange, request)
        } catch {
          case t: Throwable => {
            val err = ValidationError(s"Error while calling restful: ${t.getMessage}", ValidationError.Internal)
            IO.pure(restful.error(List(err), err.status))
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
}

object RestfulHandler {
  private val key: String = "restful"

//  def store(exchange: HttpExchange, json: Json): Unit = {
//    val merged = Json.merge(exchange.store.getOrElse[Json](key, obj()), json)
//    exchange.store.update[Json](key, merged)
//  }

  def validate[Request](request: Request,
                        validations: List[RestfulValidation[Request]]): Either[List[ValidationError], Request] = {
    var errors = ListBuffer.empty[ValidationError]
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

  def jsonFromContent(content: Content): Either[ValidationError, Json] = Right(JsonParser(content.asString))

  def jsonFromURL(url: URL): Json = {
    val entries = url.parameters.map.toList.map {
      case (key, param) => {
        val values = param.values
        val valuesJson = if (values.length > 1) {
          arr(values.map(str): _*)
        } else {
          str(values.head)
        }
        key -> valuesJson
      }
    }
    obj(entries: _*)
  }
}