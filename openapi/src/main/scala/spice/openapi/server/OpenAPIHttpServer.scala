package spice.openapi.server

import cats.effect.IO
import scribe.mdc.MDC
import spice.http.HttpExchange
import spice.http.content.Content
import spice.http.server.HttpServer
import spice.openapi._
import spice.net._
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIInfo, OpenAPIPath, OpenAPISchema, OpenAPIServer, OpenAPITag}

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, VectorMap}

trait OpenAPIHttpServer extends HttpServer {
  def openAPIVersion: String = "3.0.3"

  def title: String
  def version: String
  def description: Option[String] = None
  def tags: List[String] = Nil

  def api: OpenAPI = OpenAPI(
    openapi = openAPIVersion,
    info = OpenAPIInfo(
      title = title,
      version = version,
      description = description
    ),
    tags = tags.map(OpenAPITag.apply),
    servers = config.listeners() flatMap { server =>
      server.urls.map { url =>
        OpenAPIServer(url = url, description = server.description)
      }
    },
    paths = services.map { service =>
      service.path.toString -> OpenAPIPath(
        parameters = Nil, // TODO: Implement
        methods = service.calls.flatMap(sc => sc.openAPI.map(sc.method -> _)).toMap
      )
    }.toMap,
    components = Some(OpenAPIComponents(
      parameters = Map.empty,
      schemas = OpenAPIHttpServer.components
    ))
  )

  def services: List[Service]

  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = services
    .to(LazyList)
    .flatMap(_(exchange))
    .headOption match {
      case Some(sc) => sc.handle(exchange)
      case None if exchange.request.url.path == path"/openapi.json" => exchange.withContent(
        Content.json(api.asJson, compact = false)
      )
      case None if exchange.request.url.path == path"/openapi.yaml" => exchange.withContent(
        Content.string(api.asYaml, ContentType.`text/yaml`)
      )
      case None => IO.pure(exchange)
    }
}

object OpenAPIHttpServer {
  private var fullNameMap = Map.empty[String, String]
  private var componentsMap = Map.empty[String, OpenAPISchema]

  def register(fullName: String)(f: => OpenAPISchema): String = synchronized {
    fullNameMap.get(fullName) match {
      case Some(name) => name
      case None =>
        val schema: OpenAPISchema = f
        val name = determineAvailableName(fullName)
        fullNameMap += fullName -> name
        componentsMap += name -> schema
        name
    }
  }

  def components: Map[String, OpenAPISchema] = VectorMap(componentsMap.toList.sortBy(_._1): _*)

  private def determineAvailableName(fullName: String): String = {
    val index = fullName.lastIndexOf('.')
    val shortName = fullName.substring(index + 1)

    @tailrec
    def recurse(i: Int): String = {
      val n = if (i == 0) {
        shortName
      } else {
        s"$shortName$i"
      }
      if (!fullNameMap.valuesIterator.contains(n)) {
        n
      } else {
        recurse(i + 1)
      }
    }

    recurse(0)
  }
}