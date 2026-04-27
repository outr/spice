package spice.openapi.server

import rapid.*
import fabric.*
import fabric.define.{DefType, Definition}
import fabric.io.JsonParser
import fabric.rw.*
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpMethod}
import spice.http.server.BasePath
import spice.http.server.handler.HttpHandler
import spice.http.server.rest.Restful
import spice.net.*
import spice.openapi.{OpenAPIContent, OpenAPIContentType, OpenAPIGenericType, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema}

import scala.util.Try

trait ServiceCall extends HttpHandler {
  type Request
  type Response
  def method: HttpMethod
  def responseTypes: List[ResponseType]

  def summary: String
  def description: String
  def tags: List[String] = Nil
  def operationId: Option[String] = None
  def successDescription: String
  def security: Option[List[Map[String, List[String]]]] = None
  def errorResponses: Map[String, OpenAPIResponse] = Map.empty

  def service: Service

  given requestRW: RW[Request]
  given responseRW: RW[Response]

  def requestSchema: Option[Schema]
  def responseSchema: Option[Schema]

  def apply(request: ServiceRequest[Request])(using mdc: MDC): Task[ServiceResponse[Response]]

  override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] = {
    // Merge the base path of the listener (if defined) to the service path
    val actualPath = BasePath.get(exchange) match {
      case Some(basePath) => basePath.merge(service.path)
      case None => service.path
    }
    val args = exchange.request.url.path.extractArguments(actualPath).toList.map {
      case (key, value) => key -> Try(JsonParser(value)).getOrElse(Str(value))
    }
    val argsJson = obj(args*)
    Restful.jsonFromExchange(exchange).flatMap { contentJson =>
      val requestJson = if (argsJson.isEmpty) {
        contentJson
      } else {
        argsJson.merge(contentJson)
      }
      val request = requestJson.as[Request]
      apply(ServiceRequest[Request](request, exchange)).map(_.exchange)
    }
  }

  private def hasFile(map: Map[String, Definition]): Boolean = {
    map.values.exists(d => d.className.contains("spice.http.server.rest.FileUpload"))
  }

  lazy val openAPI: Option[OpenAPIPathEntry] = {
    val requestDef = requestRW.definition
    val contentType = requestDef.defType match {
      case DefType.Obj(map) if hasFile(map) => ContentType.`multipart/form-data`
      case _ => ContentType.`application/json`
    }
    val requestBody = if (requestDef.defType == DefType.Null || method == HttpMethod.Get) {
      None
    } else {
      Some(OpenAPIRequestBody(
        required = true,
        content = OpenAPIContent(
          contentType -> OpenAPIContentType(
            schema = if (contentType == ContentType.`multipart/form-data`) {
              val map = requestDef.defType.asInstanceOf[DefType.Obj].map
              componentSchema(requestDef, requestSchema.getOrElse(Schema()), map, None, nullable = None)
            } else {
              schemaFrom(requestDef, requestSchema.getOrElse(Schema()), None, nullable = None)
            }
          )
        )
      ))
    }
    Some(OpenAPIPathEntry(
      summary = summary,
      description = description,
      tags = tags,
      operationId = operationId,
      requestBody = requestBody,
      security = security,
      responses = Map(
        "200" -> OpenAPIResponse(
          description = successDescription,
          content = Some(OpenAPIContent(
            responseTypes.map { rt =>
              rt.contentType -> OpenAPIContentType(
                schema = schemaFrom(responseRW.definition, responseSchema.getOrElse(Schema()), rt.format, nullable = None)
              )
            }*
          ))
        )
      ) ++ errorResponses
    ))
  }

  private def toOpenAPIGenericTypes(d: Definition): List[OpenAPIGenericType] =
    d.genericTypes.flatMap { gt =>
      gt.definition.className.map(cn => OpenAPIGenericType(gt.name, cn.split('.').last))
    }

  private def componentSchema(d: Definition, schema: Schema, map: Map[String, Definition], format: Option[String], nullable: Option[Boolean]): OpenAPISchema = {
    val className = d.className
    // Formal type-parameter names from the source class (e.g. `T` for
    // `Auth[T]`). Distinct from `Ref.genericTypeArgs`, which carries the
    // resolved arguments at use sites — code generators use this to emit
    // parameterized class declarations.
    val typeParams = d.genericTypes.map(_.name)
    val c = if (map.keySet == Set("[key]")) {
      val t = map("[key]")
      OpenAPISchema.Component(
        `type` = "object",
        format = format,
        additionalProperties = Some(schemaFrom(t, Schema(), format, nullable)),
        xFullClass = className,
        xTypeParameters = typeParams
      )
    } else {
      val requiredFields = map.collect {
        case (key, d) if !d.defType.isOpt => key
      }.toList
      OpenAPISchema.Component(
        `type` = "object",
        format = format,
        properties = map.map {
          case (key, d) => key -> schemaFrom(d, schema.properties.getOrElse(key, Schema()), format, nullable)
        },
        required = requiredFields,
        xFullClass = className,
        xTypeParameters = typeParams
      )
    }
    if (nullable.getOrElse(false)) {
      c.makeNullable
    } else {
      c
    }
  }

  private def isSimpleEnum(values: Map[String, Definition]): Boolean =
    values.values.forall(_.defType == DefType.Null)

  private def schemaFrom(d: Definition, schema: Schema, format: Option[String], nullable: Option[Boolean]): OpenAPISchema = {
    val className = d.className
    val description = d.description
    (d.defType match {
      case DefType.Obj(_) if className.contains("spice.http.server.rest.FileUpload") => OpenAPISchema.Component(
        `type` = "string",
        format = Some("binary")
      )
      case DefType.Obj(map) if className.isEmpty => componentSchema(d, schema, map, format, nullable)
      case DefType.Obj(map) =>
        val cn = className.get
        val refName = service.server.register(cn)(componentSchema(d, schema, map, format, None))
        OpenAPISchema.Ref(s"#/components/schemas/$refName", nullable, toOpenAPIGenericTypes(d))
      case DefType.Poly(values) if isSimpleEnum(values) && className.isDefined =>
        val cn = className.get
        val enumValues = values.keys.map(s => fabric.Str(s)).toList
        val refName = service.server.register(cn)(OpenAPISchema.Component(
          `type` = "string",
          description = description.orElse(Some(cn)),
          `enum` = enumValues,
          format = format,
          nullable = nullable,
          xFullClass = Some(cn)
        ))
        OpenAPISchema.Ref(s"#/components/schemas/$refName", nullable)
      case DefType.Poly(values) if isSimpleEnum(values) =>
        val enumValues = values.keys.map(s => fabric.Str(s)).toList
        OpenAPISchema.Component(
          `type` = "string",
          description = description.orElse(className),
          `enum` = enumValues,
          format = format,
          nullable = nullable,
          xFullClass = className
        )
      case DefType.Arr(t) => OpenAPISchema.Component(
        `type` = "array",
        description = description,
        format = format,
        items = Some(schemaFrom(t, schema.items.getOrElse(Schema()), format, None)),
        nullable = nullable
      )
      case DefType.Str => OpenAPISchema.Component(
        `type` = "string",
        format = format,
        nullable = nullable
      )
      case DefType.Bool => OpenAPISchema.Component(
        `type` = "boolean",
        format = format,
        nullable = nullable
      )
      case DefType.Int => OpenAPISchema.Component(
        `type` = "integer",
        format = format,
        nullable = nullable
      )
      case DefType.Dec => OpenAPISchema.Component(
        `type` = "number",
        format = format,
        nullable = nullable
      )
      case DefType.Opt(t) => schemaFrom(t, schema, format = format, nullable = Some(true))
      case DefType.Null => OpenAPISchema.Component(
        `type` = "null",
        format = format
      )
      case DefType.Poly(values) =>
        val schemas = values.map { case (name, innerDef) =>
          name -> schemaFrom(innerDef, schema, format, None)
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
          nullable = nullable,
          description = description
        )
        className match {
          case Some(cn) =>
            val refName = service.server.register(cn)(oneOf)
            OpenAPISchema.Ref(s"#/components/schemas/$refName", nullable)
          case None => oneOf
        }
      case DefType.Json => OpenAPISchema.Component(
        `type` = "json",
        format = format,
        nullable = nullable
      )
    }) match {
      case c: OpenAPISchema.Component => c.withDefinition(d).withSchema(schema)
      case other => other.withSchema(schema)
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