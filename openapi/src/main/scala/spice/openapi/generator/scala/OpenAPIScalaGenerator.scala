package spice.openapi.generator.scala

import fabric.Str
import spice.openapi.{OpenAPI, OpenAPISchema}
import spice.openapi.generator.{OpenAPIGenerator, OpenAPIGeneratorConfig, SourceFile}

/**
 * Generates Scala 3 models (fabric `derives RW`) from an OpenAPI spec.
 *
 * The other generators in this package emit clients for a spec spice itself produced, describing its
 * own services. This one exists for the opposite case: consuming somebody ELSE's published API from
 * Scala, where the models are currently hand-written and drift silently as the upstream contract moves.
 *
 * That difference shows up in one place that matters. A spec spice emits marks an optional field
 * `nullable`, because that is what fabric's `Option[T]` becomes; a published spec instead lists the
 * mandatory ones in `required`. Treating "not nullable" as "mandatory" against such a spec would make
 * nearly every field mandatory and the generated models unusable, so optionality here is
 * `nullable || !required.contains(name)` and the `required` list wins when it is present.
 *
 * Emission follows the conventions these models are written in by hand: one type per file, Scala 3
 * `enum` with an explicit wire value, `Option[T] = None` for optional fields, and `derives RW`.
 */
case class OpenAPIScalaGenerator(api: OpenAPI,
                                 config: OpenAPIGeneratorConfig,
                                 basePackage: String = "client",
                                 /** Fabric collapses Scala `Long` to `DefType.Int`, so a spice-emitted
                                   * spec cannot mark int64. Callers name the fields that really are
                                   * `Long` (epoch stamps, ids, money); `*Millis`/`*Seconds` always are. */
                                 int64Fields: Set[String] = Set.empty) extends OpenAPIGenerator {
  private lazy val oneOfParentMap: Map[String, String] = config.buildOneOfParentMap(api)
  private lazy val enumValueToTypeMap: Map[String, String] = config.buildEnumValueToTypeMap(api)

  private val modelPackage: String = s"$basePackage.model"
  private val modelPath: String = modelPackage.replace('.', '/')

  override protected def fileExtension: String = ".scala"
  override protected def generatedComment: String = "// GENERATED CODE: Do not edit!"

  override def generate(): List[SourceFile] = generateModels()

  // ---- names --------------------------------------------------------------

  private def refName(ref: String): String = ref.substring(ref.lastIndexOf('/') + 1)
  private def className(cn: String): String = ScalaNames.className(cn)

  private def typeNameForComponent(rawKey: String, c: OpenAPISchema.Component): String =
    className(c.xFullClass.getOrElse(rawKey))

  private def refToType(ref: String): String = api.componentByRef(ref) match {
    // A component that carries nothing but a primitive type is not emitted (there would be nothing in
    // the file), so a reference to one has to resolve to that primitive. Emitting the name instead
    // produced source referring to a type that was never written - `genreId` is declared
    // `type: string` and every `$ref` to it named a missing `GenreId`.
    case Some(c: OpenAPISchema.Component) if isPrimitiveOnly(c) => primitive(c.`type`, c.format, refName(ref))
    // A named MAP (object + additionalProperties, no properties of its own) is an alias, not a class.
    // Emitting `case class ShowMap()` for it compiled perfectly well and then silently deserialized
    // every show in the payload to nothing - which is why this is caught by round-tripping real data
    // and not by the compiler.
    case Some(c: OpenAPISchema.Component) if isMapAlias(c) =>
      s"Map[String, ${scalaType(refName(ref), c.additionalProperties.get)}]"
    case Some(c: OpenAPISchema.Component) => typeNameForComponent(refName(ref), c)
    case _ => className(refName(ref))
  }

  /** Flatten an `allOf` into one component: the union of every referenced and inline part.
    *
    * Composition has no direct expression as a case class, and the alternative - emitting nothing -
    * left references to the composed type dangling (`service` is `serviceInfo` plus two properties,
    * and every `$ref` to it named a `Service` that was never generated). Merging keeps the generated
    * model faithful to what the payload actually contains. */
  private def flattenAllOf(a: OpenAPISchema.AllOf): OpenAPISchema.Component = {
    val parts = a.schemas.flatMap {
      case r: OpenAPISchema.Ref => api.componentByRef(r.ref).collect { case c: OpenAPISchema.Component => c }
      case c: OpenAPISchema.Component => Some(c)
      case _ => None
    }
    OpenAPISchema.Component(
      `type` = "object",
      description = a.description.orElse(parts.flatMap(_.description).headOption),
      properties = parts.foldLeft(Map.empty[String, OpenAPISchema])((acc, c) => acc ++ c.properties),
      required = parts.flatMap(_.required).distinct
    )
  }

  // ---- types --------------------------------------------------------------

  private def primitive(t: String, format: Option[String], fieldName: String): String = t match {
    case "string" => "String"
    case "boolean" => "Boolean"
    case "number" => "Double"
    case "integer" =>
      if (format.contains("int64") || int64Fields.contains(fieldName) ||
        fieldName.endsWith("Millis") || fieldName.endsWith("Seconds")) "Long" else "Int"
    case "json" => "fabric.Json"
    case "null" => "Unit"
    case other => throw new RuntimeException(s"Unsupported primitive type: $other")
  }

  private def scalaType(fieldName: String, schema: OpenAPISchema): String = schema match {
    case c: OpenAPISchema.Component if c.`enum`.nonEmpty =>
      // An inline enum belongs to a named component elsewhere in the spec; reuse that type rather
      // than emitting an anonymous copy per field.
      enumValueToTypeMap.get(c.`enum`.head.asString).map(className).getOrElse("String")
    case c: OpenAPISchema.Component if c.`type` == "array" =>
      s"List[${scalaType(fieldName, c.items.getOrElse(throw new RuntimeException(s"array $fieldName is missing items")))}]"
    case c: OpenAPISchema.Component if c.`type` == "object" && c.additionalProperties.nonEmpty =>
      s"Map[String, ${scalaType(fieldName, c.additionalProperties.get)}]"
    case c: OpenAPISchema.Component if c.`type` == "object" && c.properties.isEmpty => "fabric.Json"
    case c: OpenAPISchema.Component => primitive(c.`type`, c.format, fieldName)
    case r: OpenAPISchema.Ref => refToType(r.ref)
    case o: OpenAPISchema.OneOf =>
      o.schemas.collect { case ref: OpenAPISchema.Ref => oneOfParentMap.get(refToType(ref.ref)) }.flatten.distinct match {
        case p :: Nil => p
        // A oneOf with no shared parent is a plain union; fabric has no encoding for that, so the
        // honest fallback is raw Json rather than an arbitrary pick.
        case _ => "fabric.Json"
      }
    // allOf composes schemas. Emitting the single ref case keeps the common "one $ref plus overrides"
    // shape correct; anything richer is raw Json rather than a wrong type.
    case a: OpenAPISchema.AllOf => a.schemas.collect { case r: OpenAPISchema.Ref => refToType(r.ref) } match {
      case t :: Nil => t
      case _ => "fabric.Json"
    }
    case _ => "fabric.Json"
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

  /**
   * Whether a property is optional.
   *
   * `required` wins when the schema declares one, because that is how a published spec states it. A
   * spice-emitted schema has no `required` list at all, so it falls through to `nullable` and behaves
   * exactly as the Kotlin and Dart generators do.
   */
  private def optional(owner: OpenAPISchema.Component, wire: String, schema: OpenAPISchema): Boolean =
    if (owner.required.nonEmpty) !owner.required.contains(wire) else nullable(schema)

  // ---- emission -----------------------------------------------------------

  private def header: String = s"$generatedComment\npackage $modelPackage\n\nimport fabric.rw.*\n"

  private def file(name: String, source: String): SourceFile =
    SourceFile(language = "Scala", name = name, fileName = s"$name.scala", path = modelPath, source = source)

  private def generateModels(): List[SourceFile] = {
    val schemas = api.components.toList.flatMap(_.schemas.toList)
    schemas.flatMap {
      case (key, c: OpenAPISchema.Component) if c.`enum`.nonEmpty => Some(emitEnum(typeNameForComponent(key, c), c))
      case (key, c: OpenAPISchema.Component) if isTypedWrapper(key, c) => Some(emitWrapper(typeNameForComponent(key, c), c))
      case (_, c: OpenAPISchema.Component) if isPrimitiveOnly(c) => None
      case (_, c: OpenAPISchema.Component) if isMapAlias(c) => None
      case (key, c: OpenAPISchema.Component) => Some(emitCaseClass(key, c))
      case (key, o: OpenAPISchema.OneOf) => Some(emitSealed(key, o))
      case (key, a: OpenAPISchema.AllOf) => Some(emitCaseClass(key, flattenAllOf(a)))
      // Unlike the Kotlin generator this does not throw: a third-party spec contains shapes nobody
      // designed for us, and one unusable schema must not cost the caller every other model.
      case _ => None
    }
  }

  private def isTypedWrapper(key: String, c: OpenAPISchema.Component): Boolean =
    c.properties.isEmpty && c.`enum`.isEmpty && c.xFullClass.isDefined &&
      c.`type` != "object" && !oneOfParentMap.contains(typeNameForComponent(key, c))

  private def isPrimitiveOnly(c: OpenAPISchema.Component): Boolean =
    c.properties.isEmpty && c.`enum`.isEmpty && c.xFullClass.isEmpty && c.`type` != "object"

  /** A named map alias: an object whose shape is entirely `additionalProperties`. Has no members of
    * its own, so there is no class to emit - references resolve straight to `Map[String, X]`. */
  private def isMapAlias(c: OpenAPISchema.Component): Boolean =
    c.`type` == "object" && c.properties.isEmpty && c.`enum`.isEmpty && c.additionalProperties.nonEmpty

  private def doc(description: Option[String], indent: String = ""): String =
    description.map(_.trim).filter(_.nonEmpty) match {
      case None => ""
      case Some(d) =>
        val lines = d.split('\n').toList.map(l => s"$indent  * ${l.trim}").mkString("\n")
        s"$indent/**\n$lines\n$indent  */\n"
    }

  private def emitEnum(name: String, c: OpenAPISchema.Component): SourceFile = {
    val values = c.`enum`.collect { case Str(v, _) => v }
    val cases = values.map(v => s"""  case ${ScalaNames.enumCaseName(v)} extends $name("$v")""").mkString("\n")
    val src =
      s"""$header
         |${doc(c.description)}enum $name(val wire: String) {
         |$cases
         |}
         |
         |object $name {
         |  given RW[$name] = RW.enumeration[$name](list = values.toList, asString = _.wire)
         |
         |  def apply(wire: String): Option[$name] = values.find(_.wire == wire)
         |}
         |""".stripMargin
    file(name, src)
  }

  private def emitWrapper(name: String, c: OpenAPISchema.Component): SourceFile = {
    val underlying = primitive(c.`type`, c.format, name)
    val src = s"""$header
                 |${doc(c.description)}opaque type $name = $underlying
                 |
                 |object $name {
                 |  given RW[$name] = summon[RW[$underlying]]
                 |
                 |  def apply(value: $underlying): $name = value
                 |
                 |  extension (v: $name) def value: $underlying = v
                 |}
                 |""".stripMargin
    file(name, src)
  }

  private def emitCaseClass(key: String, c: OpenAPISchema.Component): SourceFile = {
    val name = typeNameForComponent(key, c)
    val parent = oneOfParentMap.get(name)
    val params = c.properties.toList.map { case (wire, schema) =>
      val base = scalaType(wire, schema)
      val isOptional = optional(c, wire, schema)
      // A collection defaults to empty rather than becoming an Option: an absent array and an empty
      // one mean the same thing to every consumer, and Option[List[_]] pushes that distinction onto
      // callers who then have to invent an answer for it.
      val (tpe, default) =
        if (!isOptional) (base, "")
        else if (base.startsWith("List[")) (base, " = Nil")
        else if (base.startsWith("Map[")) (base, " = Map.empty")
        else (s"Option[$base]", " = None")
      (ScalaNames.propName(wire), tpe, default, isOptional)
    }
    // Mandatory first: a parameter with a default cannot precede one without.
    val ordered = params.sortBy(_._4)
    val body = ordered.map { case (n, t, d, _) => s"                 $n: $t$d" }
      .mkString("\n", ",\n", "").replaceFirst("^\\n {17}", "")
    val extendsClause = parent.map(p => s" extends $p").getOrElse("")
    val src =
      if (c.properties.isEmpty)
        s"""$header
           |${doc(c.description)}case class $name()$extendsClause derives RW
           |""".stripMargin
      else
        s"""$header
           |${doc(c.description)}case class $name($body)$extendsClause derives RW
           |""".stripMargin
    file(name, src)
  }

  private def emitSealed(key: String, o: OpenAPISchema.OneOf): SourceFile = {
    val name = className(key)
    val children = o.schemas.collect { case r: OpenAPISchema.Ref => refToType(r.ref) }
    val cases = children.map(child => s"  * [[$child]]").mkString("\n")
    val src =
      s"""$header
         |/** One of:
         |$cases
         |  */
         |sealed trait $name derives RW
         |""".stripMargin
    file(name, src)
  }
}
