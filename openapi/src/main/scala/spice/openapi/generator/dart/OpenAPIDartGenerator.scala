package spice.openapi.generator.dart

import fabric.io.JsonFormatter
import fabric.rw.Convertible
import fabric.{Json, Str}
import spice.http.HttpMethod
import spice.net.ContentType
import spice.openapi.{OpenAPI, OpenAPIContent, OpenAPISchema}
import spice.openapi.generator.{OpenAPIGenerator, OpenAPIGeneratorConfig, SourceFile}
import spice.streamer.*
import spice.streamer.given
import spice.util.NumberToWords

import scala.collection.mutable

case class OpenAPIDartGenerator(api: OpenAPI, config: OpenAPIGeneratorConfig) extends OpenAPIGenerator {
  private lazy val baseForTypeMap: Map[String, String] = config.buildBaseForTypeMap(api)
  private lazy val resolvedBaseNames: List[(String, Set[String])] = config.buildBaseNames(api)

  private lazy val ModelTemplate: String = loadString("generator/dart/model.template")
  private lazy val ModelWithParamsTemplate: String = loadString("generator/dart/model_with_params.template")
  private lazy val ParentTemplate: String = loadString("generator/dart/parent.template")
  private lazy val EnumTemplate: String = loadString("generator/dart/enum.template")
  private lazy val ServiceTemplate: String = loadString("generator/dart/service.template")

  override protected def fileExtension: String = ".dart"

  override protected def generatedComment: String = "/// GENERATED CODE: Do not edit!"

  extension (s: String) {
    private def ref: String = s.substring(s.lastIndexOf('/') + 1)

    private def ref2Type: String = {
      // Convert any FQN-style ref name into a Dart-safe class name (concatenated class chain).
      // Without this, refs to OneOf/AllOf/AnyOf/Not components — whose map keys are FQNs —
      // would emit raw dotted identifiers as both Dart type names and file paths.
      def safeName: String = if (ref.contains('.')) dartNameForFullClass(ref) else ref
      api.componentByRef(s) match {
        case Some(c: OpenAPISchema.Component) => typeNameForComponent(ref, c)
        case Some(_: OpenAPISchema.Ref) => safeName
        case Some(_: OpenAPISchema.OneOf) => safeName
        case Some(_: OpenAPISchema.AllOf) => safeName
        case Some(_: OpenAPISchema.AnyOf) => safeName
        case Some(_: OpenAPISchema.Not) => safeName
        case _ => safeName
      }
    }
    private def ref2DartType(r: OpenAPISchema.Ref): String = {
      api.componentByRef(s) match {
        case Some(c: OpenAPISchema.Component) =>
          dartTypeWithGenerics(typeNameForComponent(ref, c), r)
        case _ => ref2Type
      }
    }
    private def type2File: String = {
      // Always derive the file name from the Dart class name, never from a raw FQN.
      val name = ref2Type
      val pre = name.charAt(0).toLower
      val suffix = "\\p{Lu}".r.replaceAllIn(name.substring(1), m => {
        s"_${m.group(0).toLowerCase}"
      })
      s"$pre$suffix".replace(" ", "")
    }
    private def dartType: String = s match {
      case "string" => "String"
      case "boolean" => "bool"
      case "integer" => "int"
      case "number" => "double"
      case "json" => "Map<String, dynamic>"
      case _ => throw new RuntimeException(s"Unsupported dart type: [$s]")
    }
    private def param: String = s"this.$prop"
    private def prop: String = renameMap.getOrElse(s, s)
  }

  extension (content: OpenAPIContent) {
    private def ref: OpenAPISchema.Ref = content
      .content
      .head
      ._2
      .schema
      .asInstanceOf[OpenAPISchema.Ref]

    private def refType: String = {
      val t = ref.ref.ref2Type
      typeNameForComponent(t, component)
    }

    /** Like `refType`, but includes resolved generic args — `Foo<Bar>` rather
     * than just `Foo`. Use this for parameter / return-type positions in the
     * generated service method signatures so callers see the typed shape. */
    private def refTypeWithGenerics: String = {
      val base = refType
      if (ref.genericTypeArgs.isEmpty) base
      else s"$base<${ref.genericTypeArgs.map(_.typeName).mkString(", ")}>"
    }

    private def component: OpenAPISchema.Component = api.componentByRef(ref.ref).get match {
      case c: OpenAPISchema.Component => c
      case _ => throw new RuntimeException(s"Expected Component schema but got: ${api.componentByRef(ref.ref)}")
    }
  }

  /** Factory plumbing for generic types referenced by the service emitter.
   *
   * When a request/response type is parameterized in Scala (e.g.
   * `SimpleAuthenticatedRequest[User]`), the generated Dart class is
   * `SimpleAuthenticatedRequest<User>` with `genericArgumentFactories: true`.
   * Its `toJson(...)` and `fromJson(...)` require closures that handle each
   * type-arg's own (de)serialization. Service-method emission threads those
   * closures here.
   */
  private def toJsonFactoryCalls(ref: OpenAPISchema.Ref): String =
    ref.genericTypeArgs.map(_ => "(v) => v.toJson()").mkString(", ")

  private def fromJsonFactoryCalls(ref: OpenAPISchema.Ref,
                                    imports: mutable.Set[String]): String =
    ref.genericTypeArgs.map { gt =>
      safeAddImport(imports, gt.typeName)
      s"(e) => ${gt.typeName}.fromJson(e as Map<String, dynamic>)"
    }.mkString(", ")

  /** `request.toJson()` if non-generic, `request.toJson(<factories>)` otherwise. */
  private def requestToJsonExpr(ref: OpenAPISchema.Ref): String =
    if (ref.genericTypeArgs.isEmpty) "request.toJson()"
    else s"request.toJson(${toJsonFactoryCalls(ref)})"

  /** Function reference for `restful` / `restPost` — bare `Type.fromJson` if
   * non-generic, or a wrapping closure that supplies the factories otherwise. */
  private def responseFromJsonExpr(ref: OpenAPISchema.Ref,
                                    responseTypeName: String,
                                    imports: mutable.Set[String]): String = {
    if (ref.genericTypeArgs.isEmpty) s"$responseTypeName.fromJson"
    else s"(json) => $responseTypeName.fromJson(json, ${fromJsonFactoryCalls(ref, imports)})"
  }

  private lazy val renameMap = Map(
    "bool" -> "b",
    "_id" -> "id"
  )

  /** Track wrapper type names (e.g., "Id", "Timestamp") discovered during generation */
  private val wrapperTypes = mutable.Set.empty[String]

  private def field(`type`: String, name: String, nullable: Boolean): String = {
    val rename = renameMap.get(name)
    val n = if (nullable) "?" else ""
    rename match {
      case Some(r) => s"@JsonKey(name: '$name') final ${`type`}$n $r;"
      case None => s"final ${`type`}$n $name;"
    }
  }

  override def generate(): List[SourceFile] = {
    val service = generateService()
    val modelObjects = generatePaths()

    service :: modelObjects
  }

  private var parentFiles = Map.empty[String, SourceFile]

  def generatePaths(): List[SourceFile] = {
    parentFiles = Map.empty
    val sourceFiles = api.components.toList.flatMap(_.schemas.toList).filter {
      case (typeName, schema: OpenAPISchema.Component) =>
        !isPrimitiveTypeOnly(typeName, schema) && !conflictsWithDartBuiltInTypes(typeName)
      case (typeName, _: OpenAPISchema.OneOf) => !conflictsWithDartBuiltInTypes(typeName)
      case (typeName, _: OpenAPISchema.AllOf) => !conflictsWithDartBuiltInTypes(typeName)
      case (typeName, _: OpenAPISchema.AnyOf) => !conflictsWithDartBuiltInTypes(typeName)
      case (typeName, _: OpenAPISchema.Not) => !conflictsWithDartBuiltInTypes(typeName)
      case _ => false
    }.flatMap {
      case (typeName, schema: OpenAPISchema.Component) => Some(parseComponent(typeName, schema))
      case (typeName, schema: OpenAPISchema.OneOf) => parseOneOf(typeName, schema)
      case (typeName, schema: OpenAPISchema.AllOf) => Some(parseAllOf(typeName, schema))
      case (typeName, schema: OpenAPISchema.AnyOf) => Some(parseAnyOf(typeName, schema))
      case (typeName, schema: OpenAPISchema.Not) => Some(parseNot(typeName, schema))
      case (typeName, schema) => throw new UnsupportedOperationException(s"$typeName has unsupported schema: $schema")
    }
    sourceFiles ::: parentFiles.values.toList
  }

  /**
   * Check if a schema is just a primitive type without additional properties that would warrant a separate Dart file
   */
  private def isPrimitiveTypeOnly(typeName: String, schema: OpenAPISchema.Component): Boolean = {
    // If it has properties, it's not just a primitive type
    if (schema.properties.nonEmpty) return false

    // If it has an enum, it should generate a file (even if it's a primitive type)
    if (schema.`enum`.nonEmpty) return false

    // If it has additional properties like description, format, etc., it might need a file
    if (schema.description.isDefined || schema.format.isDefined || schema.example.isDefined) return false

    // If it has validation constraints, it might need a file
    if (schema.minLength.isDefined || schema.maxLength.isDefined || schema.pattern.isDefined ||
        schema.minimum.isDefined || schema.maximum.isDefined || schema.exclusiveMinimum.isDefined ||
        schema.exclusiveMaximum.isDefined || schema.multipleOf.isDefined) return false

    // If it has xFullClass, it should generate a file (typed wrapper like Id, Timestamp)
    if (schema.xFullClass.isDefined) return false

    // If it's a polymorphic subtype (has a parent in the type map), it needs a class
    if (baseForTypeMap.contains(dartNameForFullClass(typeName))) return false

    // Otherwise, it's just a primitive type that doesn't need a separate file
    true
  }

  private def conflictsWithDartBuiltInTypes(typeName: String): Boolean =
    OpenAPIDartGenerator.DartBuiltInTypes.contains(typeName.toLowerCase)

  /**
   * Safely add an import, filtering out any that would conflict with Dart's built-in types
   */
  private def safeAddImport(imports: mutable.Set[String], typeName: String): Unit = {
    if (!conflictsWithDartBuiltInTypes(typeName)) {
      imports += typeName
    }
  }

  /** Look up a target component's directory path by its Dart class name (e.g., "Auth" → "lib/model/spec").
    * Handles all OpenAPISchema variants — Components (via xFullClass), and non-Component schemas
    * (OneOf/AllOf/AnyOf/Not) where the path comes from the FQN-shaped components map key. */
  private def pathForDartType(dartType: String): String = {
    api.components.toList.flatMap(_.schemas.toList).collectFirst {
      case (_, c: OpenAPISchema.Component) if typeNameForComponent("", c) == dartType =>
        modelPathForSchema(c)
      case (key, _) if key.contains('.') && dartNameForFullClass(key) == dartType =>
        // FQN-keyed non-Component schema (typically a OneOf parent)
        modelPathFor(key)
      case (key, _) if key == dartType =>
        // Non-FQN key (e.g., a plain "Status") — lives at the model root
        "lib/model"
    }.orElse(
      parentFiles.get(dartType).map(_.path)
    ).getOrElse("lib/model")
  }

  /** Render an import statement with the relative path from the source location to the target type. */
  private def renderImport(dartType: String, fromPath: String): String = {
    val toPath = pathForDartType(dartType)
    val fileName = s"${dartType.type2File}.dart"
    s"import '${relativeImport(fromPath, toPath, fileName)}';"
  }

  private def parseOneOf(rawTypeName: String, schema: OpenAPISchema.OneOf): Option[SourceFile] = {
    val typeName = if (rawTypeName.contains('.')) dartNameForFullClass(rawTypeName) else rawTypeName
    val parentPath = if (rawTypeName.contains('.')) modelPathFor(rawTypeName) else "lib/model"
    // If all schemas are $ref, generate a proper parent class via addParent (goes to parentFiles)
    val refs = schema.schemas.collect { case ref: OpenAPISchema.Ref => ref }
    if (refs.size == schema.schemas.size) {
      addParent(typeName, fullClassName = Some(rawTypeName))
      None // Will be included from parentFiles
    } else {
      // Mixed or inline schemas — generate a stub abstract class
      val fileName = s"${typeName.type2File}.dart"
      val source = s"""/// GENERATED CODE: Do not edit!
                      |import 'package:equatable/equatable.dart';
                      |import 'package:json_annotation/json_annotation.dart';
                      |
                      |part '${typeName.type2File}.g.dart';
                      |
                      |@JsonSerializable()
                      |abstract class $typeName extends Equatable {
                      |  const $typeName();
                      |
                      |  factory $typeName.fromJson(Map<String, dynamic> json) {
                      |    // This is a base class for OneOf schema - implement specific logic in subclasses
                      |    throw UnimplementedError('Implement in specific subclasses');
                      |  }
                      |
                      |  Map<String, dynamic> toJson();
                      |
                      |  @override
                      |  List<Object?> get props => [];
                      |}""".stripMargin

      Some(SourceFile(
        language = "Dart",
        name = typeName,
        fileName = fileName,
        path = parentPath,
        source = source
      ))
    }
  }

  /** Generate a stub abstract class for AllOf/AnyOf/Not — placeholder schemas with composition semantics
    * not yet fully implemented. Uses path-based file location derived from the rawTypeName. */
  private def parseCompositionStub(rawTypeName: String, kind: String, comment: String): SourceFile = {
    val typeName = if (rawTypeName.contains('.')) dartNameForFullClass(rawTypeName) else rawTypeName
    val path = if (rawTypeName.contains('.')) modelPathFor(rawTypeName) else "lib/model"
    val fileName = s"${typeName.type2File}.dart"
    val source = s"""/// GENERATED CODE: Do not edit!
                    |import 'package:equatable/equatable.dart';
                    |import 'package:json_annotation/json_annotation.dart';
                    |
                    |part '${typeName.type2File}.g.dart';
                    |
                    |@JsonSerializable()
                    |class $typeName extends Equatable {
                    |  // $comment
                    |  const $typeName();
                    |
                    |  factory $typeName.fromJson(Map<String, dynamic> json) {
                    |    return _$$${typeName}FromJson(json);
                    |  }
                    |
                    |  Map<String, dynamic> toJson() => _$$${typeName}ToJson(this);
                    |
                    |  @override
                    |  List<Object?> get props => [];
                    |}""".stripMargin

    SourceFile(
      language = "Dart",
      name = typeName,
      fileName = fileName,
      path = path,
      source = source
    )
  }

  private def parseAllOf(typeName: String, schema: OpenAPISchema.AllOf): SourceFile =
    parseCompositionStub(typeName, "AllOf", "This class combines multiple schemas - implement specific fields based on the schemas")

  private def parseAnyOf(typeName: String, schema: OpenAPISchema.AnyOf): SourceFile =
    parseCompositionStub(typeName, "AnyOf", "This class can be any of the specified types - implement based on your needs")

  private def parseNot(typeName: String, schema: OpenAPISchema.Not): SourceFile =
    parseCompositionStub(typeName, "Not", "This class excludes the specified schema - implement based on your needs")

  private def stripTypeArgs(s: String): String = {
    val idx = s.indexOf('[')
    if (idx == -1) s else s.substring(0, idx)
  }

  /** Splits a className into (package segments, class chain segments).
    * Package segments are leading lowercase-first parts; class chain segments are uppercase-first parts. */
  private def splitClassName(cn: String): (List[String], List[String]) = {
    val clean = stripTypeArgs(cn).replace("$", ".")
    val parts = clean.split('.').toList.filter(_.nonEmpty)
    val (pkg, cls) = parts.span(p => p.charAt(0).isLower)
    (pkg, cls)
  }

  /** Compute the directory path under `lib/model` for a class, e.g.:
    * "com.example.foo.Bar" → "lib/model/com/example/foo"
    * "spec.OpenAPIHttpServerSpec.Auth" → "lib/model/spec" */
  private def modelPathFor(cn: String): String = {
    val (pkg, _) = splitClassName(cn)
    if (pkg.isEmpty) "lib/model" else s"lib/model/${pkg.mkString("/")}"
  }

  /** Compute the directory path for the schema's xFullClass (or fall back to "lib/model"). */
  private def modelPathForSchema(schema: OpenAPISchema.Component): String =
    schema.xFullClass.map(modelPathFor).getOrElse("lib/model")

  /** Compute relative path from one directory to another file. Both paths are absolute starting with "lib/". */
  private def relativeImport(fromDir: String, toDir: String, fileName: String): String = {
    if (fromDir == toDir) fileName
    else {
      val fromParts = fromDir.split("/").toList.filter(_.nonEmpty)
      val toParts = toDir.split("/").toList.filter(_.nonEmpty)
      val commonLen = fromParts.zip(toParts).takeWhile { case (a, b) => a == b }.length
      val ups = "../" * (fromParts.length - commonLen)
      val downs = toParts.drop(commonLen).mkString("/")
      if (downs.nonEmpty) s"$ups$downs/$fileName" else s"$ups$fileName"
    }
  }

  /** Convert a full Scala className to its Dart class name by concatenating the class chain segments. */
  private def dartNameForFullClass(cn: String): String = {
    val (_, classChain) = splitClassName(cn)
    if (classChain.nonEmpty) classChain.mkString else cn.replace(" ", "").replace(".", "")
  }

  /** Extract the wire discriminator value from a full Scala className.
    * Matches Fabric's `Product.productPrefix` — just the leaf class name.
    * "spec.OpenAPIHttpServerSpec.Auth" → "Auth"
    * "sigil.event.Message" → "Message" */
  private def wireDiscriminator(cn: String): String = {
    val (_, classChain) = splitClassName(cn)
    classChain.lastOption.getOrElse(cn.replace(" ", ""))
  }

  /** Look up the FQN for a generated Dart class name by scanning components. */
  private def fqnForDartType(dartType: String): Option[String] = {
    api.components.toList.flatMap(_.schemas.toList).collectFirst {
      case (key, c: OpenAPISchema.Component) if typeNameForComponent(key, c) == dartType =>
        c.xFullClass.getOrElse(key)
      case (key, _) if key.contains('.') && dartNameForFullClass(key) == dartType => key
    }
  }

  private def typeNameForComponent(rawTypeName: => String, schema: OpenAPISchema.Component): String = schema.xFullClass match {
    case Some(cn) => dartNameForFullClass(cn)
    case None =>
      val raw = rawTypeName
      // If rawTypeName looks like a full className (contains dots), derive Dart name from it
      if (raw.contains('.')) dartNameForFullClass(raw)
      else raw.replace(" ", "")
  }

  private def dartTypeWithGenerics(baseName: String, ref: OpenAPISchema.Ref): String = {
    if (ref.genericTypeArgs.isEmpty) baseName
    else s"$baseName<${ref.genericTypeArgs.map(_.typeName).mkString(", ")}>"
  }

  private def parseEnum(typeName: String, `enum`: List[String], path: String = "lib/model"): SourceFile = {
    val fileName = s"${typeName.type2File}.dart"
    val fields = `enum`.map { e =>
      val className = e.filter(_.isLetterOrDigit) match {
        case s if s.charAt(0).isDigit =>
          val digits = s.takeWhile(_.isDigit)
          val nums = NumberToWords(digits.toInt)
          s"${nums.filter(_.isLetter)}${s.substring(digits.length)}"
        case s => s
      }
      s"""@JsonValue('$e')
         |
         |  $className('$e')""".stripMargin
    }.mkString(",\n  ")
    val source = EnumTemplate
      .replace("%%FILENAME%%", typeName.type2File)
      .replace("%%CLASSNAME%%", typeName)
      .replace("%%FIELDS%%", fields)
    SourceFile(
      language = "Dart",
      name = typeName,
      fileName = fileName,
      path = path,
      source = source
    )
  }

  private def parseTypedWrapper(typeName: String, schema: OpenAPISchema.Component): SourceFile = {
    val dartType = schema.`type` match {
      case "string" => "String"
      case "integer" => "int"
      case "number" => "double"
      case "boolean" => "bool"
      case "object" => "Object"
      case other => other
    }
    val fileName = s"${typeName.type2File}.dart"
    val source =
      s"""import 'package:json_annotation/json_annotation.dart';
         |
         |/// GENERATED CODE: Do not edit!
         |/// Typed wrapper for $dartType
         |class $typeName {
         |  final $dartType value;
         |  const $typeName(this.value);
         |
         |  factory $typeName.fromJson($dartType value) => $typeName(value);
         |  $dartType toJson() => value;
         |
         |  @override
         |  String toString() => value.toString();
         |
         |  @override
         |  bool operator ==(Object other) => other is $typeName && other.value == value;
         |
         |  @override
         |  int get hashCode => value.hashCode;
         |}
         |""".stripMargin
    SourceFile(
      language = "Dart",
      name = typeName,
      fileName = fileName,
      path = modelPathForSchema(schema),
      source = source
    )
  }

  private def parseComponent(rawTypeName: String, schema: OpenAPISchema.Component): SourceFile = {
    val typeName: String = typeNameForComponent(rawTypeName, schema)
    val componentPath: String = modelPathForSchema(schema)
    if (schema.`enum`.nonEmpty) {
      val `enum` = schema.`enum`.map {
        case Str(s, _) => s
        case json => throw new UnsupportedOperationException(s"Enum only supports Str: $json")
      }
      parseEnum(typeName, `enum`, componentPath)
    } else if (schema.properties.isEmpty && schema.`type`.nonEmpty && schema.xFullClass.isDefined && !baseForTypeMap.contains(typeName)) {
      // Simple type wrapper (e.g., Id wrapping String, Timestamp wrapping int) — not a poly subtype
      parseTypedWrapper(typeName, schema)
    } else {
      val imports = mutable.Set.empty[String]

      val fileName: String = s"${typeName.type2File}.dart"
      val fields: List[String] = schema.properties.toList.map {
        case (fieldName, schema) => parseField(fieldName, schema, imports, componentPath).toString
      }
      val fieldsString = fields.mkString("\n  ") match {
        case "" => "// No fields defined"
        case s => s
      }
      val parent = baseForTypeMap.get(typeName)
      val extending = parent match {
        case Some(parentName) =>
          imports += parentName
          s"extends $parentName with EquatableMixin "
        case None => "extends Equatable "
      }
      val importsTemplate = imports.toList.sorted.map(t => renderImport(t, componentPath)).mkString("\n") match {
        case "" => "// No imports necessary"
        case s => s
      }
      val paramsList = schema.properties.toList.map {
        case (fieldName, schema) =>
          val nullable = schema match {
            case s: OpenAPISchema.Component => s.nullable.getOrElse(false)
            case s: OpenAPISchema.Ref => s.nullable.getOrElse(false)
            case s: OpenAPISchema.OneOf => s.nullable.getOrElse(false)
            case _ => throw new RuntimeException(s"Unsupported OpenAPISchema: $schema")
          }
          if (nullable) {
            fieldName.param
          } else {
            s"required ${fieldName.param}"
          }
      }
      val params = if (paramsList.nonEmpty) {
        paramsList.mkString("{", ", ", "}")
      } else {
        ""
      }
      val props = schema.properties.toList.map(_._1.prop).mkString(", ")

      // ---- Generic-class plumbing ----------------------------------------
      // When the source Scala class is parameterized (e.g. `Auth[T]`), emit a
      // matching Dart declaration `class Auth<T>` and thread factory functions
      // through fromJson / toJson per json_serializable's
      // `genericArgumentFactories` contract. For each formal parameter `X` we
      // add a positional `fromJsonX: X Function(Object?)` argument to fromJson
      // and `toJsonX: Object? Function(X)` to toJson. These are forwarded to
      // the build_runner-generated `_$XxxFromJson` / `_$XxxToJson` so any
      // T-typed nested fields can be re-hydrated.
      //
      // For phantom parameters (T appears only on the Scala side via type
      // class constraints — common in this codebase, where `Id[T]` collapses
      // to `String`), the factory is unused at runtime but the signatures
      // still need to be in place so call sites can pass them.
      //
      // **Polymorphic exception:** if the class has a non-generic abstract
      // parent (i.e. `parent.isDefined`), we suppress the type parameter on
      // the child here. Otherwise the child's overridden toJson signature
      // wouldn't match the parent's abstract zero-arg `toJson()`. The
      // upstream Scala trait would need to be generic too for this to work
      // cleanly, which is an outr-core change. Until then the child class
      // is type-erased — acceptable while there's only one concrete User /
      // Organization in the consuming app.
      val typeParamsList: List[String] = if (parent.isDefined) Nil else schema.xTypeParameters
      val isGeneric: Boolean = typeParamsList.nonEmpty
      val typeParamDecl: String =
        if (isGeneric) typeParamsList.mkString("<", ", ", ">") else ""
      val classNameWithParams: String =
        if (isGeneric) s"$typeName$typeParamDecl" else typeName

      val annotations: String =
        if (isGeneric) "@JsonSerializable(explicitToJson: true, genericArgumentFactories: true)"
        else "@JsonSerializable(explicitToJson: true)"

      val fromJsonFactoryParams: String =
        if (isGeneric) typeParamsList.map(t => s"$t Function(Object?) fromJson$t").mkString(", ") else ""
      val fromJsonFactoryArgs: String =
        if (isGeneric) typeParamsList.map(t => s"fromJson$t").mkString(", ") else ""
      val toJsonFactoryParams: String =
        if (isGeneric) typeParamsList.map(t => s"Object? Function($t) toJson$t").mkString(", ") else ""
      val toJsonFactoryArgs: String =
        if (isGeneric) typeParamsList.map(t => s"toJson$t").mkString(", ") else ""

      // For generic classes we emit a `factory` constructor (rather than a
      // `static` method) because build_runner's json_serializable specifically
      // looks for a factory `ClassName.fromJson` when wiring up nested generic
      // calls under `genericArgumentFactories: true`. With a static method,
      // build_runner emits a single-arg call and the build fails.
      val fromJson: String = if (isGeneric) {
        s"""factory $typeName.fromJson(
           |    Map<String, dynamic> json,
           |    $fromJsonFactoryParams,
           |  ) => _$$${typeName}FromJson(json, $fromJsonFactoryArgs);""".stripMargin
      } else {
        s"static $typeName fromJson(Map<String, dynamic> json) => _$$${typeName}FromJson(json);"
      }

      val toJson: String = (parent, isGeneric) match {
        case (Some(_), false) =>
          s"""@override Map<String, dynamic> toJson() {
             |    Map<String, dynamic> map = _$$${typeName}ToJson(this);
             |    map['type'] = '${config.discriminatorValue(wireDiscriminator(rawTypeName))}';
             |    return map;
             |  }""".stripMargin
        case (Some(_), true) =>
          // Discriminator-bearing AND generic — uncommon, but emit the
          // factory-aware shape and still inject the discriminator.
          s"""@override Map<String, dynamic> toJson($toJsonFactoryParams) {
             |    Map<String, dynamic> map = _$$${typeName}ToJson(this, $toJsonFactoryArgs);
             |    map['type'] = '${config.discriminatorValue(wireDiscriminator(rawTypeName))}';
             |    return map;
             |  }""".stripMargin
        case (None, true) =>
          s"Map<String, dynamic> toJson($toJsonFactoryParams) => _$$${typeName}ToJson(this, $toJsonFactoryArgs);"
        case (None, false) =>
          s"Map<String, dynamic> toJson() => _$$${typeName}ToJson(this);"
      }

      // deepClone: round-trip through json. For generic classes we don't have
      // the type-erased factories at hand, so route the clone through
      // `copyWith()` which copy_with_extension provides natively (and which
      // already respects type parameters).
      val deepCloneSnippet: String = if (isGeneric) {
        s"$classNameWithParams deepClone() => copyWith.call();"
      } else {
        s"$typeName deepClone() => fromJson(toJson());"
      }

      // Pick the right template for the static body, then patch in the
      // generic-aware members after-the-fact. ModelTemplate is for parameterless
      // constructors; both templates support `%%CLASSNAME%%` interpolation.
      // For generic classes we inject `<T>` into key positions via the
      // %%CLASSNAME_DECL%% placeholder when present in the template.
      val source = (if (params.isEmpty) ModelTemplate else ModelWithParamsTemplate)
        .replace("%%IMPORTS%%", importsTemplate)
        .replace("%%FILENAME%%", typeName.type2File)
        .replace("%%ANNOTATIONS%%", annotations)
        .replace("%%CLASSNAME_DECL%%", classNameWithParams)
        .replace("%%CLASSNAME%%", typeName)
        .replace("%%EXTENDS%%", extending)
        .replace("%%FIELDS%%", fieldsString)
        .replace("%%PARAMS%%", params)
        .replace("%%PROPS%%", props)
        .replace("%%FROMJSON%%", fromJson)
        .replace("%%TOJSON%%", toJson)
        .replace("%%DEEPCLONE%%", deepCloneSnippet)
      SourceFile(
        language = "Dart",
        name = typeName,
        fileName = fileName,
        path = componentPath,
        source = source
      )
    }
  }

  private def parseField(fieldName: String,
                         schema: OpenAPISchema,
                         imports: mutable.Set[String],
                         fromPath: String = "lib/model"): ParsedField = schema match {
    case c: OpenAPISchema.Component if c.`enum`.nonEmpty =>
      val parentName = typeNameForComponent(
        rawTypeName = baseForTypeMap.getOrElse(c.`enum`.head.asString, throw new NullPointerException(s"Unable to find enum entry ${c.`enum`.head} for $fieldName")),
        schema = c
      )
      val `enum` = c.`enum`.map {
        case Str(s, _) => s
        case json => throw new UnsupportedOperationException(s"Enum only supports Str: $json")
      }
      safeAddImport(imports, parentName)
      addParent(parentName, `enum`)
      ParsedField(parentName, fieldName, c.nullable.getOrElse(false))
    case c: OpenAPISchema.Component if c.`type` == "array" =>
      val item = parseField(fieldName, c.items.get, imports)
      item.copy(
        `type` = s"List<${item.`type`}>",
        nullable = c.nullable.getOrElse(false)
      )
    case c: OpenAPISchema.Component if c.`type` == "object" =>
      val fieldType = if (c.`enum`.nonEmpty) {
        val `enum` = c.`enum`.map {
          case Str(s, _) => s
          case json => throw new UnsupportedOperationException(s"Enum only supports Str: $json")
        }
        val parentName = typeNameForComponent(
          rawTypeName = baseForTypeMap.getOrElse(c.`enum`.head.asString, throw new NullPointerException(s"Unable to find enum entry ${c.`enum`.head} for $fieldName")),
          schema = c
        )
        safeAddImport(imports, parentName)
        addParent(parentName, `enum`)
        parentName.dartType
      } else if (c.`type` == "object") {
        if (c.additionalProperties.nonEmpty) {
          val additionalField = parseField(
            fieldName = fieldName,
            schema = c.additionalProperties.get,
            imports = imports
          )
          s"Map<String, ${additionalField.`type`}>"
        } else {
          throw new RuntimeException(s"Failed to parse: ${JsonFormatter.Default(c.json)}")
        }
      } else {
        c.`type`.dartType
      }
      ParsedField(fieldType, fieldName, c.nullable.getOrElse(false))
    case c: OpenAPISchema.Component =>
      ParsedField(c.`type`.dartType, fieldName, c.nullable.getOrElse(false))
    case c: OpenAPISchema.Ref =>
      val modelType = c.ref.ref2Type
      val dartType = c.ref.ref2DartType(c)
      safeAddImport(imports, modelType)
      // Also import each resolved generic argument so `Auth<User>` (and similar)
      // compiles — the generic arg is a concrete type name, not a phantom.
      c.genericTypeArgs.foreach { gt =>
        safeAddImport(imports, gt.typeName)
      }
      ParsedField(dartType, fieldName, c.nullable.getOrElse(false))
    case c: OpenAPISchema.OneOf =>
      val refs = c.schemas.map(_.asInstanceOf[OpenAPISchema.Ref].ref.ref2Type)
      val parents: List[String] = refs.map(r => baseForTypeMap.getOrElse(r, throw new RuntimeException(s"No mapping defined for $r"))).distinct
      val parentName = parents match {
        case parent :: Nil => parent
        case _ => throw new RuntimeException(s"Multiple parents found for ${refs.mkString(", ")}: ${parents.mkString(", ")}")
      }
      safeAddImport(imports, parentName)
      addParent(parentName)
      ParsedField(parentName, fieldName, c.nullable.getOrElse(false))
    case _ => throw new UnsupportedOperationException(s"Schema for '$fieldName' is unsupported: $schema")
  }

  private def addParent(tn: String, `enum`: List[String] = Nil, fullClassName: Option[String] = None): Unit = {
    val typeName = tn.replace(" ", "")
    val parentPath = fullClassName.map(modelPathFor).getOrElse("lib/model")
    if (!parentFiles.contains(typeName)) {
      if (`enum`.nonEmpty) {
        parentFiles += typeName -> parseEnum(typeName, `enum`, parentPath)
      } else {
        // Find children: try both Dart-name and full-className lookup in baseNames
        val children = resolvedBaseNames.find(_._1 == typeName.ref).map(_._2.toList.sorted)
          .orElse(fullClassName.flatMap(fcn => resolvedBaseNames.find(_._1 == fcn).map(_._2.toList.sorted)))
          .getOrElse(throw new RuntimeException(s"Unable to find children for parent $typeName (full: $fullClassName)"))
        val typedChildren = children.map(_.ref2Type)
        var imports = mutable.Set.empty[String]
        imports ++= typedChildren
        
        // children are Dart class names (from buildBaseNames); components.schemas is keyed by
        // the full Scala className. Resolve each Dart name to its FQN before lookup.
        def lookupComponent(child: String): OpenAPISchema.Component = {
          val components = api.components.get
          val key = fqnForDartType(child).getOrElse(child)
          components.schemas.getOrElse(key,
            throw new RuntimeException(s"Unable to find component for child '$child' (looked up as '$key'). Available: ${components.schemas.keys.mkString(", ")}")
          ) match {
            case c: OpenAPISchema.Component => c
            case other =>
              throw new RuntimeException(s"Expected OpenAPISchema.Component for '$child', got: ${other.getClass.getSimpleName}")
          }
        }

        // Also collect imports from all child schemas, not just common properties
        // This ensures that types like PhoneNumberType are imported even if they're not in common properties
        children.foreach { child =>
          val component = lookupComponent(child)
          component.properties.foreach {
            case (_, schema) =>
              schema match {
                case ref: OpenAPISchema.Ref =>
                  val refType = ref.ref.ref2Type
                  safeAddImport(imports, refType)
                case _ => // Handle other schema types if needed
              }
          }
        }

        val maps = children.map { child =>
          val component = lookupComponent(child)
          component.properties.map {
            case (key, schema) =>
              def recurseType(schema: OpenAPISchema): String = schema match {
                case c: OpenAPISchema.Component if c.`type` == "array" =>
                  s"List<${recurseType(c.items.get)}>${if (c.nullable.contains(true)) "?" else ""}"
                case c: OpenAPISchema.Component if c.`enum`.nonEmpty =>
                  val parentName = baseForTypeMap(c.`enum`.head.asString)
                  val `enum` = c.`enum`.map {
                    case Str(s, _) => s
                    case json => throw new UnsupportedOperationException(s"Enum only supports Str: $json")
                  }
                  safeAddImport(imports, parentName)
                  addParent(parentName, `enum`)

                  if (c.nullable.contains(true)) {
                    s"$parentName?"
                  } else {
                    parentName
                  }
                case c: OpenAPISchema.Component if c.nullable.contains(true) => s"${c.`type`.dartType}?"
                case c: OpenAPISchema.Component => c.`type`.dartType
                case r: OpenAPISchema.Ref =>
                  // Resolve the ref via ref2Type so FQN-keyed components produce a Dart class name
                  // (e.g., "com.outr.model.phone.PhoneNumberType" → "PhoneNumberType"), not the raw FQN.
                  val c = r.ref.ref2Type
                  safeAddImport(imports, c)
                  if (r.nullable.contains(true)) {
                    s"$c?"
                  } else {
                    c
                  }
                case o: OpenAPISchema.OneOf =>
                  val refs = o.schemas.map {
                    case ref: OpenAPISchema.Ref => ref.ref.ref2Type
                    case c: OpenAPISchema.Component =>
                      scribe.info(JsonFormatter.Default(c.json))
                      typeNameForComponent(c.`type`.dartType, c)
                    case s => throw new UnsupportedOperationException(s"Failure: $s")
                  }
                  val parents: List[String] = refs.map(r => baseForTypeMap.getOrElse(r, throw new RuntimeException(s"No mapping defined for $r"))).distinct
                  val parentName = parents match {
                    case parent :: Nil => parent
                    case _ => throw new RuntimeException(s"Multiple parents found for ${refs.mkString(", ")}: ${parents.mkString(", ")}")
                  }
                  safeAddImport(imports, parentName)
                  addParent(parentName)
                  if (o.nullable.contains(true)) {
                    s"$parentName?"
                  } else {
                    parentName
                  }
                case s => throw new RuntimeException(s"Unsupported schema: $s")
              }
              val `type` = recurseType(schema)
              val k = key match {
                case "_id" => "id"
                case _ => key
              }
              k -> `type`
          }
        }
        val commonKeys = maps.map(_.keySet).reduce(_ intersect _)
        val baseParams = commonKeys.map(key => key -> maps.head(key)).toMap
        val fields = baseParams.toList.map {
          case (key, value) => s"$value get $key;";
        }.mkString("\n  ")
        val fileName = s"${typeName.type2File}.dart"
        val importString = imports.map(t => renderImport(t, parentPath)).mkString("\n")
        val fromJson = children.zip(typedChildren).map {
          case (child, typed) =>
            // Discriminator matches Fabric's wire format: simple class name (Product.productPrefix).
            // `child` is a Dart class name; recover the FQN to extract the leaf simple name.
            val simpleLeaf = fqnForDartType(child).map(wireDiscriminator).getOrElse(child)
            val discriminator = config.discriminatorValue(simpleLeaf)
            s"""if (t == '$discriminator') {
               |      return $typed.fromJson(json);
               |    }""".stripMargin
        }.mkString("    ", " else ",
          """ else {
            |      throw Exception("Unsupported type: $t");
            |    }""".stripMargin)
        val source = ParentTemplate
          .replace("%%CLASSNAME%%", typeName)
          .replace("%%FIELDS%%", fields)
          .replace("%%IMPORTS%%", importString)
          .replace("%%FROMJSON%%", fromJson)
        parentFiles += typeName -> SourceFile(
          language = "Dart",
          name = typeName,
          fileName = fileName,
          path = parentPath,
          source = source
        )
      }
    }
  }

  def generateService(): SourceFile = {
    var imports = mutable.Set.empty[String]
    val methods: List[String] = api.paths.toList.sortBy(_._1).flatMap {
      case (pathString, path) =>
        val name = "[^a-zA-Z0-9](\\S)".r.replaceAllIn(pathString.substring(1), m => {
          m.group(1).toUpperCase
        })
        // Handle GET endpoints (no request body)
        if (path.methods.contains(HttpMethod.Get)) {
          val entry = path.methods(HttpMethod.Get)
          val successResponse = entry.responses("200").content.get
          val (_, apiPath) = successResponse.content.head
          val (responseType, responseTypeWithGenerics, fromJsonExpr) = apiPath.schema match {
            case c: OpenAPISchema.Ref =>
              val rt = c.ref.ref2Type
              safeAddImport(imports, rt)
              c.genericTypeArgs.foreach(gt => safeAddImport(imports, gt.typeName))
              val withG = if (c.genericTypeArgs.isEmpty) rt
                          else s"$rt<${c.genericTypeArgs.map(_.typeName).mkString(", ")}>"
              (rt, withG, responseFromJsonExpr(c, rt, imports))
            case c: OpenAPISchema.Component if c.`type` == "object" && c.properties.nonEmpty =>
              val rt = c.xFullClass.map(cn => cn.substring(cn.lastIndexOf('.') + 1)).getOrElse(name.capitalize + "Response")
              (rt, rt, s"$rt.fromJson")
            case _ => ("Map<String, dynamic>", "Map<String, dynamic>", "Map<String, dynamic>.fromJson")
          }
          Some(s"""  /// ${entry.description}
             |  static Future<$responseTypeWithGenerics> $name() async {
             |    return await restGet(
             |      "$pathString",
             |      $fromJsonExpr
             |    );
             |  }""".stripMargin)
        } else if (path.methods.contains(HttpMethod.Post)) {
        val entry = path.methods(HttpMethod.Post)
        val requestContent = entry.requestBody.get.content
        val (requestContentType, apiContentType) = requestContent.content.head
        val successResponse = entry
          .responses("200")
          .content.get
        val (_, apiPath) = successResponse.content.head
        Some(apiPath.schema match {
          case c: OpenAPISchema.Component if c.`type` == "null" =>
            // void return type — fire and forget
            val requestRef = requestContent.ref
            val requestType = requestContent.refTypeWithGenerics
            safeAddImport(imports, requestContent.refType)
            requestRef.genericTypeArgs.foreach(gt => safeAddImport(imports, gt.typeName))
            s"""  /// ${entry.description}
               |  static Future<void> $name($requestType request) async {
               |    await restPost(
               |      "$pathString",
               |      ${requestToJsonExpr(requestRef)}
               |    );
               |  }""".stripMargin
          case c: OpenAPISchema.Component => c.format match {
            case Some("binary") =>
              val requestRef = requestContent.ref
              val requestType = requestContent.refTypeWithGenerics
              safeAddImport(imports, requestContent.refType)
              requestRef.genericTypeArgs.foreach(gt => safeAddImport(imports, gt.typeName))
              s"""  /// ${entry.description}
                 |  static Future<void> $name($requestType request, String fileName) async {
                 |    await restDownload(fileName, "$pathString", ${requestToJsonExpr(requestRef)});
                 |  }""".stripMargin
            case format => throw new RuntimeException(s"Unsupported schema format: $format (schema: $c, path: $pathString)")
          }
          case c: OpenAPISchema.Ref if c.ref.ref2Type == "Content" =>
            // Response is spice.http.content.Content — emit a browser download stub.
            // The match resolves the ref via ref2Type so it survives the FQN component-key
            // refactor (where the ref is now `#/components/schemas/spice.http.content.Content`).
            val requestRef = requestContent.ref
            val requestType = requestContent.refTypeWithGenerics
            safeAddImport(imports, requestContent.refType)
            requestRef.genericTypeArgs.foreach(gt => safeAddImport(imports, gt.typeName))
            s"""  /// ${entry.description}
               |  static Future<void> $name($requestType request, String fileName) async {
               |    await restDownload(fileName, "$pathString", ${requestToJsonExpr(requestRef)});
               |  }""".stripMargin
          case _: OpenAPISchema.Ref =>
            val responseRef = successResponse.ref
            val responseType = successResponse.refType
            val responseTypeWithGenerics = successResponse.refTypeWithGenerics
            val component = successResponse.component
            val binary = component.format.contains("binary")
            safeAddImport(imports, responseType)
            responseRef.genericTypeArgs.foreach(gt => safeAddImport(imports, gt.typeName))
            if (requestContentType == ContentType.`multipart/form-data`) {
              apiContentType.schema match {
                case c: OpenAPISchema.Component =>
                  val params = c.properties.toList.map {
                    case (key, schema) =>
                      val paramType = schema match {
                        case child: OpenAPISchema.Component if child.format.contains("binary") => "PlatformFile"
                        case child: OpenAPISchema.Component if child.`enum`.nonEmpty =>
                          val parentName = baseForTypeMap.getOrElse(child.`enum`.head.asString, throw new NullPointerException(s"Unable to find enum entry ${child.`enum`.head} for $key"))
                          safeAddImport(imports, parentName)
                          parentName
                        case child: OpenAPISchema.Component => s"${child.`type`.dartType}"
                        case ref: OpenAPISchema.Ref => ref.ref.ref2DartType(ref)
                        case _ => throw new UnsupportedOperationException(s"Unsupported schema for $key: $schema")
                      }
                      s"$paramType $key"
                  }.mkString(", ")
                  val ws = "        "
                  val conversions = c.properties.toList.map {
                    case (key, schema: OpenAPISchema.Component) =>
                      if (schema.format.contains("binary")) {
                        s"${ws}request.files.add(http.MultipartFile.fromBytes('$key', $key.bytes!, filename: $key.name));"
                      } else {
                        s"${ws}request.fields['$key'] = json.encode($key);"
                      }
                    case (key, ref: OpenAPISchema.Ref) =>
                      safeAddImport(imports, ref.ref.ref2Type)
                      ref.genericTypeArgs.foreach(gt => safeAddImport(imports, gt.typeName))
                      val toJsonCall =
                        if (ref.genericTypeArgs.isEmpty) s"$key.toJson()"
                        else s"$key.toJson(${toJsonFactoryCalls(ref)})"
                      s"${ws}request.fields['$key'] = json.encode($toJsonCall);"
                    case (key, schema) => throw new UnsupportedOperationException(s"Unable to support $key: $schema")
                  }.mkString("\n")
                  s"""  /// ${entry.description}
                     |  static Future<$responseTypeWithGenerics> $name($params) async {
                     |    return await multiPart(
                     |      "$pathString",
                     |      (request) {
                     |$conversions
                     |      },
                     |      ${responseFromJsonExpr(responseRef, responseType, imports)}
                     |    );
                     |  }""".stripMargin
                case _ => throw new UnsupportedOperationException(s"Unsupported schema: ${apiContentType.schema}")
              }
            } else if (binary) {
              val requestRef = requestContent.ref
              val requestType = requestContent.refTypeWithGenerics
              safeAddImport(imports, requestContent.refType)
              requestRef.genericTypeArgs.foreach(gt => safeAddImport(imports, gt.typeName))
              s"""  /// ${entry.description}
                 |  static Future<void> $name($requestType request) async {
                 |    return await restDownload(
                 |      downloadFileName,
                 |      "$pathString",
                 |      ${requestToJsonExpr(requestRef)}
                 |    );
                 |  }""".stripMargin
            } else {
              val requestRef = requestContent.ref
              val requestType = requestContent.refTypeWithGenerics
              safeAddImport(imports, requestContent.refType)
              requestRef.genericTypeArgs.foreach(gt => safeAddImport(imports, gt.typeName))
              s"""  /// ${entry.description}
                 |  static Future<$responseTypeWithGenerics> $name($requestType request) async {
                 |    return await restful(
                 |      "$pathString",
                 |      ${requestToJsonExpr(requestRef)},
                 |      ${responseFromJsonExpr(responseRef, responseType, imports)}
                 |    );
                 |  }""".stripMargin
            }
          case schema => throw new RuntimeException(s"Unsupported schema: $schema")
        })
        } else {
          None
        }
    }
    // service.dart lives at lib/, so all model imports are relative to lib/
    val importsTemplate = imports.toList.sorted.map(t => renderImport(t, "lib")).mkString("\n")
    val methodsTemplate = methods.mkString("\n\n")
    val source = ServiceTemplate
      .replace("%%IMPORTS%%", importsTemplate)
      .replace("%%SERVICES%%", methodsTemplate)
    SourceFile(
      language = "Dart",
      name = "Service",
      fileName = "service.dart",
      path = "lib",
      source = source
    )
  }

  def loadString(name: String): String = {
    val stream = getClass.getClassLoader.getResourceAsStream(name)
    if (stream == null) throw new RuntimeException(s"Not found: $name")
    Streamer(
      stream,
      new mutable.StringBuilder
    ).sync().toString
  }

  case class ParsedField(`type`: String, name: String, nullable: Boolean) {
    override def toString: String = field(`type`, name, nullable)
  }
}

object OpenAPIDartGenerator {
  private val DartBuiltInTypes: Set[String] = Set(
    "string", "int", "double", "bool", "boolean", "num", "dynamic", "void", "object",
    "list", "map", "set", "iterable", "future", "stream", "datetime", "duration", "regexp",
    "uri", "bigint", "symbol", "type", "function", "null"
  )
}