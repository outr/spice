package spice.openapi.generator.kotlin

import fabric.Str
import spice.http.HttpMethod
import spice.openapi.{OpenAPI, OpenAPIContent, OpenAPISchema}
import spice.openapi.generator.{OpenAPIGenerator, OpenAPIGeneratorConfig, SourceFile}

import scala.collection.mutable

/**
 * Generates a Kotlin client (kotlinx.serialization models) from an OpenAPI spec.
 *
 * The reason this exists instead of org.openapitools' Kotlin generator: Fabric ADTs surface as
 * `oneOf`+`discriminator`, and that generator can't turn those into a usable polymorphic hierarchy.
 * Here we emit a real sealed hierarchy: `@Serializable sealed interface Parent` + `@Serializable
 * @SerialName("<wire>") data class Child(...) : Parent`, which kotlinx deserializes natively using
 * its default `"type"` class discriminator (which matches Fabric's discriminator property).
 *
 * Modeled on `OpenAPIDartGenerator`. Every model lands in one flat package (`<basePackage>.model`),
 * so no cross-model imports are needed.
 */
case class OpenAPIKotlinGenerator(api: OpenAPI,
                                  config: OpenAPIGeneratorConfig,
                                  basePackage: String = "client",
                                  /** Fabric collapses Scala `Long` to `DefType.Int`, so the spec can't mark int64.
                                    * Callers pass the field names that are actually `Long` (epoch millis, ids,
                                    * money) so those map to Kotlin `Long` instead of an overflowing `Int`.
                                    * `*Millis`/`*Seconds` are always treated as `Long`. */
                                  int64Fields: Set[String] = Set.empty) extends OpenAPIGenerator {
  private lazy val oneOfParentMap: Map[String, String] = config.buildOneOfParentMap(api)
  private lazy val baseNames: List[(String, Set[String])] = config.buildBaseNames(api)
  private lazy val enumValueToTypeMap: Map[String, String] = config.buildEnumValueToTypeMap(api)

  private val modelPackage: String = s"$basePackage.model"
  private val modelPath: String = modelPackage.replace('.', '/')

  override protected def fileExtension: String = ".kt"
  override protected def generatedComment: String = "// GENERATED CODE: Do not edit!"

  override def generate(): List[SourceFile] = generateModels() :+ generateService()

  // ---- name helpers -------------------------------------------------------

  private def refName(ref: String): String = ref.substring(ref.lastIndexOf('/') + 1)
  private def className(cn: String): String = KotlinNames.className(cn)

  /** The Kotlin type name for a component, honoring xFullClass. */
  private def typeNameForComponent(rawKey: String, c: OpenAPISchema.Component): String =
    className(c.xFullClass.getOrElse(rawKey))

  /** Resolve a $ref to its emitted Kotlin class name. */
  private def refToType(ref: String): String = api.componentByRef(ref) match {
    case Some(c: OpenAPISchema.Component) => typeNameForComponent(refName(ref), c)
    case _ => className(refName(ref))
  }

  // ---- type mapping -------------------------------------------------------

  private def primitive(t: String, format: Option[String], fieldName: String): String = t match {
    case "string" => "String"
    case "boolean" => "Boolean"
    case "number" => "Double"
    case "integer" =>
      // spice emits Long as a bare `integer` (no format); epoch-millis/money fields would overflow Int.
      if (format.contains("int64") || int64Fields.contains(fieldName) ||
        fieldName.endsWith("Millis") || fieldName.endsWith("Seconds")) "Long" else "Int"
    case "json" => "kotlinx.serialization.json.JsonElement"
    case other => throw new RuntimeException(s"Unsupported primitive type: $other")
  }

  /** Kotlin type for a property schema (without the trailing `?`). */
  private def kotlinType(fieldName: String, schema: OpenAPISchema, contextual: mutable.Set[String]): String = schema match {
    case c: OpenAPISchema.Component if c.`enum`.nonEmpty =>
      val owner = enumValueToTypeMap.getOrElse(c.`enum`.head.asString,
        throw new RuntimeException(s"No enum owner for ${c.`enum`.head} (field $fieldName)"))
      className(owner)
    case c: OpenAPISchema.Component if c.`type` == "array" =>
      s"List<${kotlinTypeNullable(fieldName, c.items.getOrElse(throw new RuntimeException(s"array $fieldName missing items")), contextual)}>"
    case c: OpenAPISchema.Component if c.`type` == "object" && c.additionalProperties.nonEmpty =>
      s"Map<String, ${kotlinTypeNullable(fieldName, c.additionalProperties.get, contextual)}>"
    case c: OpenAPISchema.Component =>
      primitive(c.`type`, c.format, fieldName)
    case r: OpenAPISchema.Ref => refToType(r.ref)
    case o: OpenAPISchema.OneOf =>
      val parents = o.schemas.collect { case ref: OpenAPISchema.Ref => oneOfParentMap.get(refToType(ref.ref)) }.flatten.distinct
      parents match {
        case p :: Nil => p
        case _ => throw new RuntimeException(s"Cannot resolve oneOf parent for field $fieldName")
      }
    case s => throw new RuntimeException(s"Unsupported schema for $fieldName: $s")
  }

  /** Kotlin type with trailing `?` when the schema is nullable. */
  private def kotlinTypeNullable(fieldName: String, schema: OpenAPISchema, contextual: mutable.Set[String]): String = {
    val base = kotlinType(fieldName, schema, contextual)
    if (nullable(schema)) s"$base?" else base
  }

  private def nullable(schema: OpenAPISchema): Boolean = schema match {
    case c: OpenAPISchema.Component => c.nullable.getOrElse(false)
    case r: OpenAPISchema.Ref => r.nullable.getOrElse(false)
    case o: OpenAPISchema.OneOf => o.nullable.getOrElse(false)
    case o: OpenAPISchema.AllOf => o.nullable.getOrElse(false)
    case o: OpenAPISchema.AnyOf => o.nullable.getOrElse(false)
    case n: OpenAPISchema.Not => n.nullable.getOrElse(false)
    case _ => false
  }

  private def propName(wire: String): String = wire match {
    case "_id" => "id"
    case s => s
  }

  // ---- model emission -----------------------------------------------------

  private def generateModels(): List[SourceFile] = {
    val schemas = api.components.toList.flatMap(_.schemas.toList)
    schemas.flatMap {
      case (key, c: OpenAPISchema.Component) if c.`enum`.nonEmpty => Some(emitEnum(typeNameForComponent(key, c), c))
      case (key, c: OpenAPISchema.Component) if isTypedWrapper(key, c) => Some(emitWrapper(typeNameForComponent(key, c), c))
      case (key, c: OpenAPISchema.Component) if isPrimitiveOnly(key, c) => None
      case (key, c: OpenAPISchema.Component) => Some(emitDataClass(key, c))
      case (key, o: OpenAPISchema.OneOf) => Some(emitSealed(key, o))
      case (key, other) => throw new RuntimeException(s"$key: unsupported top-level schema $other")
    }
  }

  /** A typed scalar wrapper (e.g. lightdb `Id`, `Timestamp`): primitive + xFullClass, not a poly child. */
  private def isTypedWrapper(key: String, c: OpenAPISchema.Component): Boolean =
    c.properties.isEmpty && c.`enum`.isEmpty && c.xFullClass.isDefined &&
      c.`type` != "object" && !oneOfParentMap.contains(typeNameForComponent(key, c))

  private def isPrimitiveOnly(key: String, c: OpenAPISchema.Component): Boolean =
    c.properties.isEmpty && c.`enum`.isEmpty && c.xFullClass.isEmpty &&
      !oneOfParentMap.contains(typeNameForComponent(key, c)) && c.`type` != "object"

  private def header(pkg: String): String = s"$generatedComment\npackage $pkg\n"

  private def file(name: String, source: String): SourceFile =
    SourceFile(language = "Kotlin", name = name, fileName = s"$name.kt", path = modelPath, source = source)

  private def sanitizeEnumEntry(value: String): String = {
    val cleaned = value.map(ch => if (ch.isLetterOrDigit) ch else '_')
    if (cleaned.nonEmpty && cleaned.head.isDigit) s"_$cleaned" else cleaned
  }

  private def emitEnum(name: String, c: OpenAPISchema.Component): SourceFile = {
    val entries = c.`enum`.map {
      case Str(v, _) => s"""    @SerialName("$v") ${sanitizeEnumEntry(v)}"""
      case j => throw new RuntimeException(s"Enum supports only strings: $j")
    }.mkString(",\n")
    val src =
      s"""${header(modelPackage)}
         |import kotlinx.serialization.SerialName
         |import kotlinx.serialization.Serializable
         |
         |@Serializable
         |enum class $name {
         |$entries
         |}
         |""".stripMargin
    file(name, src)
  }

  private def emitWrapper(name: String, c: OpenAPISchema.Component): SourceFile = {
    val underlying = primitive(c.`type`, c.format, name)
    // Wire value is a bare scalar, so a typealias keeps it transparent to kotlinx.
    val src = s"""${header(modelPackage)}
                 |typealias $name = $underlying
                 |""".stripMargin
    file(name, src)
  }

  private def renderField(wire: String, schema: OpenAPISchema, contextual: mutable.Set[String],
                          overrideField: Boolean, forceNullableDefault: Boolean): String = {
    val kt = kotlinTypeNullable(wire, schema, contextual)
    val isNullable = kt.endsWith("?")
    val ovr = if (overrideField) "override " else ""
    // Always default nullable fields so kotlinx tolerates omitted keys (Fabric omits None).
    val default = if (isNullable) " = null" else ""
    val name = propName(wire)
    val serialName = if (name != wire) s"""@SerialName("$wire") """ else ""
    s"""    $serialName${ovr}val $name: $kt$default"""
  }

  private def emitDataClass(key: String, c: OpenAPISchema.Component): SourceFile = {
    val name = typeNameForComponent(key, c)
    val contextual = mutable.Set.empty[String]
    val parent = oneOfParentMap.get(name)
    val parentCommon: Set[String] = parent.map(p => parentAbstractFieldNames(parentKeyFor(p))).getOrElse(Set.empty)
    val fields = c.properties.toList.map { case (wire, schema) =>
      renderField(wire, schema, contextual, overrideField = parentCommon.contains(wire), forceNullableDefault = false)
    }
    // Stable ordering: required-ish (non-null, no default) first keeps kotlinx happy with defaults last.
    val ordered = fields.sortBy(_.contains(" = null"))
    val ann = parent match {
      case Some(_) => s"""@Serializable\n@SerialName("${config.discriminatorValue(KotlinNames.wireDiscriminator(c.xFullClass.getOrElse(key)))}")"""
      case None => "@Serializable"
    }
    val impl = parent.map(p => s" : $p").getOrElse("")
    val body = if (ordered.isEmpty) "" else ordered.mkString("\n", ",\n", "\n")
    // A data class needs >=1 param; an empty type becomes an `object` (poly child) or
    // an instantiable empty `class` (e.g. EmptyRequest).
    val decl =
      if (c.properties.isEmpty) parent match {
        case Some(p) => s"object $name : $p"
        case None => s"class $name"
      } else s"data class $name($body)$impl"
    val src =
      s"""${header(modelPackage)}
         |import kotlinx.serialization.SerialName
         |import kotlinx.serialization.Serializable
         |
         |$ann
         |$decl
         |""".stripMargin
    file(name, src)
  }

  /** Find the components-map key for a parent Kotlin class name. */
  private def parentKeyFor(parentName: String): String =
    api.components.toList.flatMap(_.schemas.toList).collectFirst {
      case (k, o: OpenAPISchema.OneOf) if className(k) == parentName => k
    }.getOrElse(parentName)

  private def childComponentsOf(parentKey: String): List[OpenAPISchema.Component] =
    api.components.get.schemas.get(parentKey) match {
      case Some(o: OpenAPISchema.OneOf) =>
        o.schemas.collect { case r: OpenAPISchema.Ref => api.componentByRef(r.ref) }.flatten.collect { case c: OpenAPISchema.Component => c }
      case _ => Nil
    }

  /** The exact field names a sealed parent declares as abstract vals: shared across all children by
    * BOTH name and identical Kotlin type. Children mark these `override`; everything else is their own.
    * (e.g. `MediaState.tmdb` is common by name but differently typed, so it is NOT a parent field.) */
  private def parentAbstractFieldNames(parentKey: String): Set[String] = {
    val children = childComponentsOf(parentKey)
    if (children.isEmpty) Set.empty
    else {
      val common = children.map(_.properties.keySet).reduce(_ intersect _)
      val ctx = mutable.Set.empty[String]
      children.head.properties.toList.collect {
        case (w, s) if common.contains(w) &&
          children.forall(_.properties.get(w).exists(cs => kotlinTypeNullable(w, cs, ctx) == kotlinTypeNullable(w, s, ctx))) => w
      }.toSet
    }
  }

  private def emitSealed(key: String, o: OpenAPISchema.OneOf): SourceFile = {
    val name = className(key)
    val contextual = mutable.Set.empty[String]
    val fieldNames = parentAbstractFieldNames(key)
    val abstractVals = childComponentsOf(key).headOption.toList.flatMap(_.properties.toList)
      .filter { case (wire, _) => fieldNames.contains(wire) }
      .map { case (wire, schema) =>
        val kt = kotlinTypeNullable(wire, schema, contextual)
        val sn = if (propName(wire) != wire) s"""@SerialName("$wire") """ else ""
        s"""    ${sn}val ${propName(wire)}: $kt"""
      }
    val body = if (abstractVals.isEmpty) "" else abstractVals.mkString("\n", "\n", "\n")
    val src =
      s"""${header(modelPackage)}
         |import kotlinx.serialization.SerialName
         |import kotlinx.serialization.Serializable
         |
         |@Serializable
         |sealed interface $name {$body}
         |""".stripMargin
    file(name, src)
  }

  // ---- service emission ---------------------------------------------------

  private def methodName(path: String): String =
    "[^a-zA-Z0-9](\\S)".r.replaceAllIn(path.substring(1), m => m.group(1).toUpperCase)

  private def contentRef(content: OpenAPIContent): Option[OpenAPISchema.Ref] =
    content.content.headOption.flatMap { case (_, ct) => ct.schema.orElse(ct.itemSchema) }.collect { case r: OpenAPISchema.Ref => r }

  private def responseSchema(entry: spice.openapi.OpenAPIPathEntry): Option[OpenAPISchema] =
    entry.responses.get("200").flatMap(_.content).flatMap(_.content.headOption).flatMap { case (_, ct) => ct.schema.orElse(ct.itemSchema) }

  private def generateService(): SourceFile = {
    val methods = api.paths.toList.sortBy(_._1).flatMap { case (pathStr, path) =>
      val nm = methodName(pathStr)
      path.methods.get(HttpMethod.Post).map { entry =>
        val reqType = entry.requestBody.flatMap(rb => contentRef(rb.content)).map(r => refToType(r.ref))
        responseSchema(entry) match {
          case Some(c: OpenAPISchema.Component) if c.`type` == "null" =>
            reqType match {
              case Some(rt) => s"""    suspend fun $nm(request: $rt): Unit =\n        postUnit("$pathStr", request, $rt.serializer())"""
              case None => s"""    suspend fun $nm(): Unit = postUnit0("$pathStr")"""
            }
          case Some(r: OpenAPISchema.Ref) =>
            val resType = refToType(r.ref)
            reqType match {
              case Some(rt) => s"""    suspend fun $nm(request: $rt): $resType =\n        post("$pathStr", request, $rt.serializer(), $resType.serializer())"""
              case None => s"""    suspend fun $nm(): $resType = get("$pathStr", $resType.serializer())"""
            }
          case _ =>
            reqType match {
              case Some(rt) => s"""    suspend fun $nm(request: $rt): JsonElement =\n        postRaw("$pathStr", request, $rt.serializer())"""
              case None => s"""    suspend fun $nm(): JsonElement = getRaw("$pathStr")"""
            }
        }
      }.orElse(path.methods.get(HttpMethod.Get).map { entry =>
        responseSchema(entry) match {
          case Some(r: OpenAPISchema.Ref) =>
            val resType = refToType(r.ref)
            s"""    suspend fun $nm(): $resType = get("$pathStr", $resType.serializer())"""
          case _ => s"""    suspend fun $nm(): JsonElement = getRaw("$pathStr")"""
        }
      })
    }
    val src =
      s"""$generatedComment
         |package $basePackage
         |
         |import $modelPackage.*
         |import kotlinx.coroutines.Dispatchers
         |import kotlinx.coroutines.withContext
         |import kotlinx.serialization.KSerializer
         |import kotlinx.serialization.json.Json
         |import kotlinx.serialization.json.JsonElement
         |import okhttp3.MediaType.Companion.toMediaType
         |import okhttp3.OkHttpClient
         |import okhttp3.Request
         |import okhttp3.RequestBody.Companion.toRequestBody
         |
         |/** Generated API client. Set [client] (with the app's auth/header interceptors) and [baseUrl]
         |  * once at startup; every method then carries the session token via that client. */
         |object Service {
         |    lateinit var client: OkHttpClient
         |    var baseUrl: String = ""
         |    val json: Json = Json {
         |        ignoreUnknownKeys = true
         |        encodeDefaults = true
         |        isLenient = true
         |    }
         |    private val jsonMedia = "application/json; charset=utf-8".toMediaType()
         |
         |    class ServiceException(val code: Int, val responseBody: String) : RuntimeException("HTTP " + code + ": " + responseBody)
         |
         |    private fun exec(request: Request): String = client.newCall(request).execute().use { resp ->
         |        val text = resp.body?.string() ?: ""
         |        if (!resp.isSuccessful) throw ServiceException(resp.code, text)
         |        text
         |    }
         |
         |    private fun <Req> bodyOf(request: Req, reqSer: KSerializer<Req>) =
         |        json.encodeToString(reqSer, request).toRequestBody(jsonMedia)
         |
         |    suspend fun <Req, Res> post(path: String, request: Req, reqSer: KSerializer<Req>, resSer: KSerializer<Res>): Res =
         |        withContext(Dispatchers.IO) {
         |            json.decodeFromString(resSer, exec(Request.Builder().url(baseUrl + path).post(bodyOf(request, reqSer)).build()))
         |        }
         |
         |    suspend fun <Req> postUnit(path: String, request: Req, reqSer: KSerializer<Req>) {
         |        withContext(Dispatchers.IO) { exec(Request.Builder().url(baseUrl + path).post(bodyOf(request, reqSer)).build()) }
         |    }
         |
         |    suspend fun postUnit0(path: String) {
         |        withContext(Dispatchers.IO) { exec(Request.Builder().url(baseUrl + path).post("{}".toRequestBody(jsonMedia)).build()) }
         |    }
         |
         |    suspend fun <Res> get(path: String, resSer: KSerializer<Res>): Res =
         |        withContext(Dispatchers.IO) {
         |            json.decodeFromString(resSer, exec(Request.Builder().url(baseUrl + path).get().build()))
         |        }
         |
         |    suspend fun <Req> postRaw(path: String, request: Req, reqSer: KSerializer<Req>): JsonElement =
         |        withContext(Dispatchers.IO) {
         |            json.parseToJsonElement(exec(Request.Builder().url(baseUrl + path).post(bodyOf(request, reqSer)).build()))
         |        }
         |
         |    suspend fun getRaw(path: String): JsonElement =
         |        withContext(Dispatchers.IO) {
         |            json.parseToJsonElement(exec(Request.Builder().url(baseUrl + path).get().build()))
         |        }
         |
         |${methods.mkString("\n\n")}
         |}
         |""".stripMargin
    SourceFile(language = "Kotlin", name = "Service", fileName = "Service.kt", path = basePackage.replace('.', '/'), source = src)
  }
}
