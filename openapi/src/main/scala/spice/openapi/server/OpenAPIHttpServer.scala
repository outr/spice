package spice.openapi.server

import fabric.define.{DefType, Definition}
import rapid.Task
import spice.http.paths
import spice.http.content.Content
import spice.http.server.MutableHttpServer
import spice.net.*
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIInfo, OpenAPIParameter, OpenAPIPath, OpenAPISchema, OpenAPIServer, OpenAPITag}

import scala.collection.immutable.VectorMap

trait OpenAPIHttpServer extends MutableHttpServer {
  private var fullNameMap = Map.empty[String, String]
  private var componentsMap = Map.empty[String, OpenAPISchema]

  override protected def initialize(): Task[Unit] = super.initialize().map { _ =>
    services.foreach { service =>
      handlers += service
    }
  }

  def register(fullName: String)(f: => OpenAPISchema): String = synchronized {
    if (!fullNameMap.contains(fullName)) {
      val schema: OpenAPISchema = f
      fullNameMap += fullName -> fullName
      componentsMap += fullName -> schema
    }
    fullName
  }

  def components: Map[String, OpenAPISchema] = VectorMap(componentsMap.toList.sortBy(_._1)*)

  def openAPIVersion: String = "3.2.0"

  def title: String
  def version: String
  def description: Option[String] = None
  def tags: List[String] = Nil

  def api: OpenAPI = {
    var pathParametersMap = Map.empty[String, OpenAPIParameter]

    val paths = services.map { service =>
      val pathParams = service.path.arguments.map { argName =>
        val paramDef = service.calls.headOption.flatMap { call =>
          call.requestRW.definition.defType match {
            case DefType.Obj(map) => map.get(argName)
            case _ => None
          }
        }
        val openAPIType = paramDef.map(_.defType) match {
          case Some(DefType.Int) => "integer"
          case Some(DefType.Dec) => "number"
          case Some(DefType.Bool) => "boolean"
          case _ => "string"
        }
        val paramKey = s"${argName}Param"
        pathParametersMap += paramKey -> OpenAPIParameter(
          description = argName,
          name = argName,
          in = "path",
          required = true,
          schema = OpenAPISchema.Component(`type` = openAPIType)
        )
        OpenAPISchema.Ref(s"#/components/parameters/$paramKey", None)
      }

      service.path.toString -> OpenAPIPath(
        parameters = pathParams,
        methods = service.calls.flatMap(sc => sc.openAPI.map(sc.method -> _)).toMap
      )
    }.toMap

    OpenAPI(
      openapi = openAPIVersion,
      info = OpenAPIInfo(
        title = title,
        version = version,
        description = description
      ),
      tags = tags.map(name => OpenAPITag(name)),
      servers = config.listeners() flatMap { server =>
        server.urls.map { url =>
          OpenAPIServer(url = url, description = server.description)
        }
      },
      paths = paths,
      components = Some(OpenAPIComponents(
        parameters = pathParametersMap,
        schemas = components
      ))
    )
  }

  def services: List[Service]

  handler.matcher(paths.exact("/openapi.json")).content(Content.json(api.asJson, compact = false))
  handler.matcher(paths.exact("/openapi.yaml")).content(Content.string(api.asYaml, ContentType.`text/yaml`))
}