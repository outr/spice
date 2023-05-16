package spice.http.server.openapi.server

import cats.effect.IO
import scribe.data.MDC
import spice.http.HttpExchange
import spice.http.server.HttpServer
import spice.http.server.openapi._

trait OpenAPIServer extends HttpServer {
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
        get = service.get.openAPI,
        post = service.post.openAPI,
        put = service.put.openAPI
      )
    }.toMap,
    components = None // TODO: Implement
  )

  def services: List[Service]

  override def apply(exchange: HttpExchange)(implicit mdc: MDC): IO[HttpExchange] = services
    .to(LazyList)
    .flatMap(_(exchange))
    .headOption match {
      case Some(sc) => sc.handle(exchange)
      case None => IO.pure(exchange)
  }
}