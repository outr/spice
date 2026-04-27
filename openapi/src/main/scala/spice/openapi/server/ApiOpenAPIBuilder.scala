package spice.openapi.server

import fabric.define.{DefType, Definition}
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
          content = Some(OpenAPIContent(
            ContentType.`application/json` -> OpenAPIContentType(schema = responseSchema)
          ))
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

  private def isSimpleEnum(values: Map[String, Definition]): Boolean =
    values.values.forall(_.defType == DefType.Null)

  private def schemaFrom(d: Definition): OpenAPISchema = {
    val className = d.className
    val description = d.description
    d.defType match {
      case DefType.Obj(map) if className.isEmpty => componentSchema(d, map)
      case DefType.Obj(map) =>
        val cn = className.get
        val refName = register(cn)(componentSchema(d, map))
        OpenAPISchema.Ref(s"#/components/schemas/$refName", None, toOpenAPIGenericTypes(d))
      case DefType.Poly(values) if isSimpleEnum(values) && className.isDefined =>
        val cn = className.get
        val enumValues = values.keys.map(s => fabric.Str(s)).toList
        val refName = register(cn)(OpenAPISchema.Component(
          `type` = "string",
          description = description.orElse(Some(cn)),
          `enum` = enumValues,
          xFullClass = Some(cn)
        ))
        OpenAPISchema.Ref(s"#/components/schemas/$refName", None)
      case DefType.Poly(values) if isSimpleEnum(values) =>
        val enumValues = values.keys.map(s => fabric.Str(s)).toList
        OpenAPISchema.Component(
          `type` = "string",
          description = description,
          `enum` = enumValues
        )
      case DefType.Arr(t) => OpenAPISchema.Component(
        `type` = "array",
        description = description,
        items = Some(schemaFrom(t))
      )
      case DefType.Str => OpenAPISchema.Component(`type` = "string")
      case DefType.Bool => OpenAPISchema.Component(`type` = "boolean")
      case DefType.Int => OpenAPISchema.Component(`type` = "integer")
      case DefType.Dec => OpenAPISchema.Component(`type` = "number")
      case DefType.Opt(t) =>
        val inner = schemaFrom(t) match {
          case ref: OpenAPISchema.Ref => ref.copy(nullable = Some(true))
          case other => other.makeNullable
        }
        applyDescription(inner, description)
      case DefType.Null => OpenAPISchema.Component(`type` = "null")
      case DefType.Json => OpenAPISchema.Component(`type` = "json")
      case DefType.Poly(values) =>
        val schemas = values.map { case (name, innerDef) =>
          val innerSchema = schemaFrom(innerDef)
          name -> innerSchema
        }
        val discriminatorMapping = schemas.collect {
          case (name, ref: OpenAPISchema.Ref) => name -> ref.ref
        }
        val oneOf = OpenAPISchema.OneOf(
          schemas = schemas.values.toList,
          discriminator = Some(OpenAPISchema.Discriminator(
            propertyName = "type",
            mapping = discriminatorMapping
          )),
          description = description
        )
        className match {
          case Some(cn) =>
            val refName = register(cn)(oneOf)
            OpenAPISchema.Ref(s"#/components/schemas/$refName", None)
          case None => oneOf
        }
    } match {
      case c: OpenAPISchema.Component => c.withDefinition(d)
      case other => other
    }
  }

  private def toOpenAPIGenericTypes(d: Definition): List[OpenAPIGenericType] =
    d.genericTypes.flatMap { gt =>
      gt.definition.className.map(cn => OpenAPIGenericType(gt.name, cn.split('.').last))
    }

  private def componentSchema(d: Definition, map: Map[String, Definition]): OpenAPISchema = {
    val className = d.className
    val description = d.description
    // Formal type-parameter names declared on the class itself (e.g. `T` for
    // `Auth[T]`). Distinct from the resolved values that appear at use-sites
    // — those flow through `Ref.genericTypeArgs` instead.
    val typeParams = d.genericTypes.map(_.name)
    if (map.keySet == Set("[key]")) {
      OpenAPISchema.Component(
        `type` = "object",
        description = description,
        additionalProperties = Some(schemaFrom(map("[key]"))),
        xFullClass = className,
        xTypeParameters = typeParams
      )
    } else {
      OpenAPISchema.Component(
        `type` = "object",
        description = description,
        properties = map.map { case (key, d) => key -> schemaFrom(d) },
        xFullClass = className,
        xTypeParameters = typeParams
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
