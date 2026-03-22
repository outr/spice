package spice.openapi.server

import fabric.define.DefType
import fabric.rw.RW
import spice.api.server.{ApiMethodDescriptor, ApiParamDescriptor}
import spice.http.HttpMethod
import spice.net.ContentType
import spice.openapi.*

object ApiOpenAPIBuilder {
  def build(title: String, version: String, basePath: String, methods: List[ApiMethodDescriptor]): OpenAPI = {
    val builder = new ApiOpenAPIBuilder(basePath)
    val paths = methods.map { m =>
      val path = s"$basePath/${m.name}"
      val entry = builder.buildPathEntry(m)
      val httpMethod = if (m.httpMethod == "get") HttpMethod.Get else HttpMethod.Post
      path -> OpenAPIPath(methods = Map(httpMethod -> entry))
    }.toMap

    OpenAPI(
      info = OpenAPIInfo(title, version),
      paths = paths,
      components = Some(OpenAPIComponents(schemas = builder.components))
    )
  }
}

private class ApiOpenAPIBuilder(basePath: String) {
  private var fullNameMap = Map.empty[String, String]
  private var componentsMap = Map.empty[String, OpenAPISchema]

  def components: Map[String, OpenAPISchema] = componentsMap

  def buildPathEntry(method: ApiMethodDescriptor): OpenAPIPathEntry = {
    val requestBody = if (method.httpMethod == "get") {
      None
    } else {
      method.requestRW match {
        case Some(rw) =>
          // Single case class param
          Some(OpenAPIRequestBody(
            required = true,
            content = OpenAPIContent(
              ContentType.`application/json` -> OpenAPIContentType(
                schema = schemaFrom(rw.definition)
              )
            )
          ))
        case None if method.params.nonEmpty =>
          // Multi-param — synthesize an object schema
          val properties = method.params.map { p =>
            p.name -> schemaFrom(p.rw.definition)
          }.toMap
          Some(OpenAPIRequestBody(
            required = true,
            content = OpenAPIContent(
              ContentType.`application/json` -> OpenAPIContentType(
                schema = OpenAPISchema.Component(
                  `type` = "object",
                  properties = properties
                )
              )
            )
          ))
        case _ => None
      }
    }

    val responseSchema = schemaFrom(method.responseRW.definition)

    OpenAPIPathEntry(
      summary = method.name,
      description = method.name,
      operationId = Some(method.name),
      requestBody = requestBody,
      responses = Map(
        "200" -> OpenAPIResponse(
          description = "Success",
          content = OpenAPIContent(
            ContentType.`application/json` -> OpenAPIContentType(schema = responseSchema)
          )
        )
      )
    )
  }

  private def register(fullName: String)(schema: => OpenAPISchema): String = synchronized {
    fullNameMap.get(fullName) match {
      case Some(name) => name
      case None =>
        val s = schema
        val name = determineAvailableName(fullName)
        fullNameMap += fullName -> name
        componentsMap += name -> s
        name
    }
  }

  private def determineAvailableName(fullName: String): String = {
    val simple = fullName.substring(fullName.lastIndexOf('.') + 1).replace("$", "")
    if (!componentsMap.contains(simple)) {
      simple
    } else {
      var i = 1
      while (componentsMap.contains(s"$simple$i")) i += 1
      s"$simple$i"
    }
  }

  private def schemaFrom(dt: DefType): OpenAPISchema = dt match {
    case DefType.Described(inner, desc) =>
      applyDescription(schemaFrom(inner), desc)
    case DefType.Obj(map, None, _) => componentSchema(None, dt.description, map)
    case DefType.Obj(map, Some(className), _) =>
      val refName = register(className)(componentSchema(Some(className), dt.description, map))
      OpenAPISchema.Ref(s"#/components/schemas/$refName", None)
    case DefType.Enum(values, Some(className), _) =>
      val refName = register(className)(OpenAPISchema.Component(
        `type` = "string",
        description = dt.description.orElse(Some(className)),
        `enum` = values,
        xFullClass = Some(className)
      ))
      OpenAPISchema.Ref(s"#/components/schemas/$refName", None)
    case DefType.Enum(values, None, _) =>
      OpenAPISchema.Component(
        `type` = "string",
        description = dt.description,
        `enum` = values
      )
    case DefType.Arr(t, _) => OpenAPISchema.Component(
      `type` = "array",
      description = dt.description,
      items = Some(schemaFrom(t))
    )
    case DefType.Str => OpenAPISchema.Component(`type` = "string")
    case DefType.Bool => OpenAPISchema.Component(`type` = "boolean")
    case DefType.Int => OpenAPISchema.Component(`type` = "integer")
    case DefType.Dec => OpenAPISchema.Component(`type` = "number")
    case DefType.Opt(t, _) =>
      val inner = schemaFrom(t) match {
        case ref: OpenAPISchema.Ref => ref.copy(nullable = Some(true))
        case other => other.makeNullable
      }
      applyDescription(inner, dt.description)
    case DefType.Null => OpenAPISchema.Component(`type` = "null")
    case DefType.Json => OpenAPISchema.Component(`type` = "json")
    case DefType.Poly(values, _, _) => OpenAPISchema.OneOf(
      schemas = values.values.map(schemaFrom).toList,
      description = dt.description
    )
    case _ => throw new UnsupportedOperationException(s"Unsupported DefType: $dt")
  }

  private def componentSchema(className: Option[String], description: Option[String], map: Map[String, DefType]): OpenAPISchema = {
    if (map.keySet == Set("[key]")) {
      OpenAPISchema.Component(
        `type` = "object",
        description = description,
        additionalProperties = Some(schemaFrom(map("[key]"))),
        xFullClass = className
      )
    } else {
      OpenAPISchema.Component(
        `type` = "object",
        description = description,
        properties = map.map { case (key, dt) => key -> schemaFrom(dt) },
        xFullClass = className
      )
    }
  }

  private def applyDescription(schema: OpenAPISchema, description: Option[String]): OpenAPISchema = {
    description match {
      case None => schema
      case Some(_) => schema match {
        case c: OpenAPISchema.Component => c.copy(description = c.description.orElse(description))
        case other => other
      }
    }
  }
}
