package spice.openapi.generator.dart

import fabric.Str as FabricStr
import fabric.define.DefType
import spice.api.server.{DurableEnumDescriptor, DurableEventDescriptor, DurableFieldDescriptor}
import spice.openapi.generator.SourceFile
import spice.streamer.*
import spice.streamer.given

import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.io.Source

/** Descriptor for a REST tool invocation. */
case class RestToolDescriptor(name: String, params: List[(String, String)])

/** Configuration for the DurableSocket Dart code generator.
  * Event descriptors come from DurableEventIntrospect — no manual lists. */
case class DurableSocketDartConfig(
  serviceName: String,
  /** Server → Client durable event kinds (from introspection) */
  eventKinds: List[DurableEventDescriptor] = Nil,
  /** Server → Client ephemeral kinds (from introspection) */
  ephemeralKinds: List[DurableEventDescriptor] = Nil,
  /** Client → Server event kinds (from introspection) */
  clientEventKinds: List[DurableEventDescriptor] = Nil,
  /** REST tools (via POST /api/invoke) */
  restTools: List[RestToolDescriptor] = Nil,
  /** ConnectionInfo fields sent during handshake */
  infoFields: List[(String, String)] = Nil,
  /** Simple enum fallback descriptors (when DefType isn't available) */
  enums: List[DurableEnumDescriptor] = Nil,
  /** Named DefType pairs for full type generation */
  defTypes: List[(String, DefType)] = Nil,
  /** When true: handler uses storedEvent / transient / ephemeral dispatch pattern */
  storedEventMode: Boolean = false,
  /** Transient event descriptors dispatched via durable channel but not persisted.
    * These need full field info for generating typed stubs and dispatch cases. */
  transientEventKinds: List[DurableEventDescriptor] = Nil
)

/** Generates Dart code for a DurableSocket client, event handler, event sender,
  * and REST client from introspected type descriptors. */
case class DurableSocketDartGenerator(config: DurableSocketDartConfig) {
  private lazy val ClientTemplate: String = loadString("generator/dart/durable_client.template")
  private lazy val HandlerTemplate: String = loadString("generator/dart/durable_event_handler.template")
  private lazy val StoredHandlerTemplate: String = loadString("generator/dart/durable_stored_event_handler.template")
  private lazy val TypesTemplate: String = loadString("generator/dart/durable_types.template")
  private lazy val SenderTemplate: String = loadString("generator/dart/durable_event_sender.template")
  private lazy val RestTemplate: String = loadString("generator/dart/durable_rest_client.template")

  private val generatedComment = "/// GENERATED CODE: Do not edit!"
  private val sn = config.serviceName
  private val dartReserved = Set("String", "int", "double", "bool", "num", "List", "Map", "Set", "Object", "dynamic", "void", "null", "true", "false")
  /** Renames for generated class names that conflict with Flutter/Dart SDK names. */
  private val dartRename = Map("Text" -> "TextContent", "Image" -> "ImageContent")
  /** Apply rename if the name conflicts with Flutter/Dart SDK. */
  private def dartName(name: String): String = dartRename.getOrElse(name, name)

  def generate(): List[SourceFile] = {
    val base = List(generateClient(), generateHandler(), generateSender(), generateRestClient())
    if (config.defTypes.nonEmpty || config.enums.nonEmpty)
      base :+ generateDefTypes()
    else
      base
  }

  def write(sourceFiles: List[SourceFile], path: Path, deleteBeforeWrite: Boolean = true): Unit = {
    if (deleteBeforeWrite) {
      sourceFiles.map(_.path).distinct.foreach { filePath =>
        val directory = path.resolve(filePath).toFile
        directory.mkdirs()
        directory.listFiles().foreach { file =>
          if (isGenerated(file)) {
            if (!file.delete()) file.deleteOnExit()
          }
        }
      }
    }
    sourceFiles.foreach { sf =>
      val filePath = path.resolve(s"${sf.path}/${sf.fileName}")
      Files.createDirectories(filePath.getParent)
      Files.writeString(filePath, sf.source)
    }
  }

  // ---------------------------------------------------------------------------
  // Client
  // ---------------------------------------------------------------------------

  private def generateClient(): SourceFile = {
    val source = ClientTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%INFO_CLASS%%", generateInfoClass())
      .replace("%%INFO_TO_JSON%%", infoToJson())

    SourceFile("Dart", s"${sn}DurableClient", s"${snakeCase(sn)}_durable_client.dart", "lib/ws/durable", source)
  }

  private def generateInfoClass(): String = {
    if (config.infoFields.isEmpty) return s"class ${sn}ConnectionInfo {\n  Map<String, dynamic> toJson() => {};\n}"
    val fields = config.infoFields.map { case (name, typ) => s"  final $typ $name;" }.mkString("\n")
    val ctorParams = config.infoFields.map { case (name, typ) =>
      if (typ.endsWith("?")) s"this.$name" else s"required this.$name"
    }.mkString(", ")
    s"""class ${sn}ConnectionInfo {
       |$fields
       |
       |  ${sn}ConnectionInfo({$ctorParams});
       |}""".stripMargin
  }

  private def infoToJson(): String = {
    val entries = config.infoFields.map { case (name, _) => s"'$name': info.$name" }.mkString(", ")
    s"{$entries}"
  }

  // ---------------------------------------------------------------------------
  // Handler — event dispatcher with typed stubs
  // ---------------------------------------------------------------------------

  private def generateHandler(): SourceFile = {
    val source = if (config.storedEventMode) generateStoredModeHandler() else generateClassicHandler()
    SourceFile("Dart", s"${sn}DurableHandler", s"${snakeCase(sn)}_durable_handler.dart", "lib/ws/durable", source)
  }

  private def generateClassicHandler(): String = {
    val eventStubs = config.eventKinds.map { ek =>
      val params = ("String conversationId" +: ek.fields.map(f => s"${dartType(f)} ${f.name}")).mkString(", ")
      s"  void ${camelCase(ek.kind)}($params) {}"
    }.mkString("\n")

    val ephemeralStubs = config.ephemeralKinds.map { ek =>
      val params = ek.fields.map(f => s"${dartType(f)} ${f.name}").mkString(", ")
      s"  void ${camelCase(ek.kind)}($params) {}"
    }.mkString("\n")

    val eventCases = config.eventKinds.map { ek =>
      val args = ("data['conversationId'] as String" +: ek.fields.map(f => argExpr(f))).mkString(", ")
      s"      case '${ek.kind}':\n        handler.${camelCase(ek.kind)}($args);\n        break;"
    }.mkString("\n")

    val ephemeralCases = config.ephemeralKinds.map { ek =>
      val args = ek.fields.map(f => argExpr(f)).mkString(", ")
      s"      case '${ek.kind}':\n        handler.${camelCase(ek.kind)}($args);\n        break;"
    }.mkString("\n")

    HandlerTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%SERVICE_SNAKE%%", snakeCase(sn))
      .replace("%%EVENT_STUBS%%", eventStubs)
      .replace("%%EPHEMERAL_STUBS%%", ephemeralStubs)
      .replace("%%EVENT_CASES%%", eventCases)
      .replace("%%EPHEMERAL_CASES%%", ephemeralCases)
  }

  private def generateStoredModeHandler(): String = {
    val transientKinds = config.transientEventKinds

    val transientStubs = transientKinds.map { ek =>
      val params = ("String conversationId" +: ek.fields.map(f => s"${dartType(f)} ${f.name}")).mkString(", ")
      s"  void ${camelCase(ek.kind)}($params) {}"
    }.mkString("\n")

    val transientCases = transientKinds.map { ek =>
      val args = ("(data['conversationId'] as String?) ?? ''" +: ek.fields.map(f => argExpr(f))).mkString(", ")
      s"      case '${ek.kind}':\n        handler.${camelCase(ek.kind)}($args);\n        break;"
    }.mkString("\n")

    val typesImport = s"${snakeCase(sn)}_types.dart"

    StoredHandlerTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%TYPES_IMPORT%%", typesImport)
      .replace("%%TRANSIENT_STUBS%%", transientStubs)
      .replace("%%TRANSIENT_CASES%%", transientCases)
  }

  // ---------------------------------------------------------------------------
  // Sender
  // ---------------------------------------------------------------------------

  private def generateSender(): SourceFile = {
    val methods = config.clientEventKinds.map { ek =>
      val params = ek.fields.map(f => s"${dartType(f)} ${f.name}").mkString(", ")
      val jsonEntries = (s"'kind': '${ek.kind}'" +: ek.fields.map(f => s"'${f.name}': ${f.name}")).mkString(", ")
      s"""  void ${camelCase(ek.kind)}($params) {
         |    push({$jsonEntries});
         |  }""".stripMargin
    }.mkString("\n\n")

    val source = SenderTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%SERVICE_SNAKE%%", snakeCase(sn))
      .replace("%%METHODS%%", methods)

    SourceFile("Dart", s"${sn}DurableSender", s"${snakeCase(sn)}_durable_sender.dart", "lib/ws/durable", source)
  }

  // ---------------------------------------------------------------------------
  // REST client
  // ---------------------------------------------------------------------------

  private def generateRestClient(): SourceFile = {
    val methods = config.restTools.map { tool =>
      val params = tool.params.map { case (n, t) => s"$t $n" }.mkString(", ")
      val argEntries = tool.params.map { case (n, _) => s"'$n': $n" }.mkString(", ")
      s"""  Future<Map<String, dynamic>> ${camelCase(tool.name)}($params) {
         |    return invoke('${tool.name}', {$argEntries});
         |  }""".stripMargin
    }.mkString("\n\n")

    val source = RestTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%METHODS%%", methods)

    SourceFile("Dart", s"${sn}RestClient", s"${snakeCase(sn)}_rest_client.dart", "lib/ws/durable", source)
  }

  // ---------------------------------------------------------------------------
  // DefType-based type generation
  // ---------------------------------------------------------------------------

  def generateDefTypes(): SourceFile = {
    val parts = mutable.ListBuffer.empty[String]

    // Collect all types that need to be emitted, recursively
    val toEmit = mutable.LinkedHashMap.empty[String, DefType]
    config.defTypes.foreach { case (name, dt) => collectTypes(name, dt, toEmit) }

    // Build child→parent map from poly types so subtypes can extend their parent
    val childToParent = mutable.Map.empty[String, String]
    toEmit.foreach { case (name, dt) =>
      dt match {
        case p: DefType.Poly =>
          p.values.foreach { case (key, subDt) =>
            val sn = shortName(key, subDt)
            if (!childToParent.contains(sn)) childToParent(sn) = name
          }
        case _ =>
      }
    }

    // Detect poly-to-poly parent relationships: if all of Poly A's subtypes
    // are also subtypes of Poly B, then A extends B in Dart.
    val polyTypes = toEmit.collect { case (name, p: DefType.Poly) => name -> p.values.map { case (key, dt) => shortName(key, dt) }.toSet }
    for ((childPolyName, childSubs) <- polyTypes; (parentPolyName, parentSubs) <- polyTypes if childPolyName != parentPolyName) {
      if (childSubs.nonEmpty && childSubs.subsetOf(parentSubs) && !childToParent.contains(childPolyName)) {
        childToParent(childPolyName) = parentPolyName
      }
    }

    toEmit.foreach { case (name, dt) =>
      if (!dartReserved.contains(name)) {
        val dn = dartName(name)
        dt match {
          case e: DefType.Enum => parts += generateDartEnum(dn, e)
          case o: DefType.Obj  => parts += generateDartClass(dn, o, toEmit, childToParent.get(name).map(dartName))
          case p: DefType.Poly => parts += generateDartPoly(dn, p, toEmit, childToParent.get(name).map(dartName))
          case _               => // primitives / arrays / opts don't generate top-level types
        }
      }
    }

    // Simple DurableEnumDescriptor fallback enums
    config.enums.foreach { ed =>
      parts += generateSimpleDartEnum(ed.name, ed.values)
    }

    val source = TypesTemplate
      .replace("%%TYPE_DEFINITIONS%%", parts.mkString("\n\n"))

    SourceFile("Dart", s"${sn}Types", s"${snakeCase(sn)}_types.dart", "lib/ws/durable", source)
  }

  /** Recursively collect named types from a DefType tree into `out` (in dependency order). */
  private def collectTypes(name: String, dt: DefType, out: mutable.LinkedHashMap[String, DefType]): Unit = {
    if (out.contains(name)) return
    dt match {
      case DefType.Described(inner, _) => collectTypes(name, inner, out)
      case e: DefType.Enum =>
        out(name) = e
      case o: DefType.Obj =>
        // Recurse into field types first
        o.map.foreach { case (_, fieldDt) => collectNestedType(fieldDt, out) }
        out(name) = o
      case p: DefType.Poly =>
        p.values.foreach { case (subName, subDt) => collectTypes(shortName(subName, subDt), subDt, out) }
        out(name) = p
      case DefType.Arr(inner, _) => collectNestedType(inner, out)
      case DefType.Opt(inner, _) => collectNestedType(inner, out)
      case _ => // primitives — no top-level type to register
    }
  }

  private def collectNestedType(dt: DefType, out: mutable.LinkedHashMap[String, DefType]): Unit = dt match {
    case DefType.Described(inner, _) => collectNestedType(inner, out)
    case DefType.Obj(_, Some(cn), _) => collectTypes(cn.split('.').last, dt, out)
    case DefType.Enum(_, Some(cn), _) => collectTypes(cn.split('.').last, dt, out)
    case DefType.Poly(_, Some(cn), _) => collectTypes(cn.split('.').last, dt, out)
    case DefType.Arr(inner, _) => collectNestedType(inner, out)
    case DefType.Opt(inner, _) => collectNestedType(inner, out)
    case _ => ()
  }

  private def shortName(key: String, dt: DefType): String = {
    dt.className match {
      case Some(cn) =>
        val short = cn.split('.').last
        // Scala 3 generates anonymous class names for parameterless enum cases (e.g., "anon.13")
        // In that case, fall back to the poly key which is the actual discriminator value
        if (short.forall(_.isDigit) || cn.contains("anon")) key else short
      case None => key
    }
  }

  private def generateDartEnum(name: String, e: DefType.Enum): String = {
    val values = e.values.map { v =>
      val raw = v match {
        case FabricStr(s, _) => s
        case other => other.toString
      }
      dartIdentifier(raw)
    }.mkString(",\n  ")
    s"""enum $name {
       |  $values;
       |
       |  static $name? fromString(String? value) {
       |    if (value == null) return null;
       |    final lower = value.toLowerCase();
       |    return $name.values.cast<$name?>().firstWhere(
       |      (v) => v?.name.toLowerCase() == lower,
       |      orElse: () => null,
       |    );
       |  }
       |}""".stripMargin
  }

  private def generateSimpleDartEnum(name: String, values: List[String]): String = {
    val vals = values.map(dartIdentifier).mkString(",\n  ")
    s"""enum $name {
       |  $vals;
       |
       |  static $name? fromString(String? value) {
       |    if (value == null) return null;
       |    final lower = value.toLowerCase();
       |    return $name.values.cast<$name?>().firstWhere(
       |      (v) => v?.name.toLowerCase() == lower,
       |      orElse: () => null,
       |    );
       |  }
       |}""".stripMargin
  }

  private def generateDartClass(
    name: String,
    o: DefType.Obj,
    knownTypes: mutable.LinkedHashMap[String, DefType],
    parentName: Option[String] = None
  ): String = {
    val extendsClause = parentName.map(p => s" extends $p").getOrElse("")
    if (o.map.isEmpty) {
      // Empty class (e.g., case object with no fields)
      s"""class $name$extendsClause {
         |  $name.fromJson(Map<String, dynamic> json);
         |}""".stripMargin
    } else {
      val fields = o.map.map { case (fname, fdt) =>
        s"  final ${defTypeToDartType(fdt)} $fname;"
      }.mkString("\n")

      val fromJsonInits = o.map.toList.map { case (fname, fdt) =>
        s"$fname = ${defTypeFromJson(fname, fdt)}"
      }.mkString(",\n        ")

      s"""class $name$extendsClause {
         |$fields
         |
         |  $name.fromJson(Map<String, dynamic> json)
         |      : $fromJsonInits;
         |}""".stripMargin
    }
  }

  private def generateDartPoly(
    name: String,
    p: DefType.Poly,
    knownTypes: mutable.LinkedHashMap[String, DefType],
    parentName: Option[String] = None
  ): String = {
    val extendsClause = parentName.map(p => s" extends $p").getOrElse("")
    val cases = p.values.map { case (key, dt) =>
      val subName = dartName(shortName(key, dt))
      s"""    if (type == '$key') return $subName.fromJson(json);"""
    }.mkString("\n")

    s"""abstract class $name$extendsClause {
       |  const $name();
       |
       |  static $name fromJson(Map<String, dynamic> json) {
       |    final type = json['type'] as String?;
       |$cases
       |    throw ArgumentError('Unknown $name type: $$type');
       |  }
       |}""".stripMargin
  }

  // ---------------------------------------------------------------------------
  // DefType → Dart type name
  // ---------------------------------------------------------------------------

  private def defTypeToDartType(dt: DefType): String = dt match {
    case DefType.Described(inner, _) => defTypeToDartType(inner)
    case DefType.Str => "String"
    case DefType.Int => "int"
    case DefType.Dec => "double"
    case DefType.Bool => "bool"
    case DefType.Json => "Map<String, dynamic>"
    case DefType.Null => "Null"
    case DefType.Arr(inner, _) => s"List<${defTypeToDartType(inner)}>"
    case DefType.Opt(inner, _) => s"${defTypeToDartType(inner)}?"
    case DefType.Enum(_, Some(cn), _) =>
      val sub = cn.split('.').last
      if (dartReserved.contains(sub)) "String" else sub
    case DefType.Obj(_, Some(cn), _) => dartName(cn.split('.').last)
    case DefType.Poly(_, Some(cn), _) => dartName(cn.split('.').last)
    case DefType.Enum(_, None, _) => "String"
    case DefType.Obj(_, None, _) => "Map<String, dynamic>"
    case DefType.Poly(_, None, _) => "dynamic"
  }

  /** Generate a Dart expression that deserializes a field from a Map<String, dynamic>. */
  private def defTypeFromJson(fieldName: String, dt: DefType): String = {
    val access = s"json['$fieldName']"
    defTypeFromJsonExpr(access, dt)
  }

  private def defTypeFromJsonExpr(access: String, dt: DefType): String = dt match {
    case DefType.Described(inner, _) => defTypeFromJsonExpr(access, inner)
    case DefType.Str => s"$access as String? ?? ''"
    case DefType.Int => s"($access as int?) ?? 0"
    case DefType.Dec => s"($access as num?)?.toDouble() ?? 0.0"
    case DefType.Bool => s"($access as bool?) ?? false"
    case DefType.Json => s"$access is Map<String, dynamic> ? $access as Map<String, dynamic> : <String, dynamic>{}"
    case DefType.Null => "null"
    case DefType.Opt(inner, _) =>
      val innerDart = defTypeToDartType(inner)
      inner match {
        case DefType.Str => s"$access as String?"
        case DefType.Int => s"$access as int?"
        case DefType.Dec => s"($access as num?)?.toDouble()"
        case DefType.Bool => s"$access as bool?"
        case DefType.Enum(_, Some(cn), _) =>
          val sub = cn.split('.').last
          if (dartReserved.contains(sub)) s"$access as String?"
          else s"$sub.fromString($access as String?)"
        case DefType.Obj(_, Some(cn), _) => s"$access != null ? ${dartName(cn.split('.').last)}.fromJson($access as Map<String, dynamic>) : null"
        case DefType.Poly(_, Some(cn), _) => s"$access != null ? ${dartName(cn.split('.').last)}.fromJson($access as Map<String, dynamic>) : null"
        case DefType.Arr(arrInner, _) =>
          val arrExpr = defTypeFromJsonExpr(access, DefType.Arr(arrInner))
          s"$access != null ? $arrExpr : null"
        case _ => s"$access as $innerDart?"
      }
    case DefType.Arr(inner, _) =>
      val innerDart = defTypeToDartType(inner)
      inner match {
        case DefType.Str | DefType.Int | DefType.Bool | DefType.Dec =>
          s"($access as List?)?.cast<$innerDart>() ?? []"
        case DefType.Obj(_, Some(cn), _) =>
          val sub = dartName(cn.split('.').last)
          s"($access as List?)?.map((e) => $sub.fromJson(e as Map<String, dynamic>)).toList() ?? []"
        case DefType.Poly(_, Some(cn), _) =>
          val sub = dartName(cn.split('.').last)
          s"($access as List?)?.map((e) => $sub.fromJson(e as Map<String, dynamic>)).toList() ?? []"
        case DefType.Arr(_, _) =>
          s"($access as List?)?.cast<$innerDart>() ?? []"
        case _ =>
          s"($access as List?)?.cast<$innerDart>() ?? []"
      }
    case DefType.Enum(_, Some(cn), _) =>
      val sub = cn.split('.').last
      if (dartReserved.contains(sub)) s"$access as String? ?? ''"
      else s"$sub.fromString($access as String?) ?? $sub.values.first"
    case DefType.Obj(_, Some(cn), _) =>
      val sub = dartName(cn.split('.').last)
      s"$sub.fromJson($access as Map<String, dynamic>)"
    case DefType.Poly(_, Some(cn), _) =>
      val sub = dartName(cn.split('.').last)
      s"$sub.fromJson($access as Map<String, dynamic>)"
    case _ => s"$access as dynamic"
  }

  // ---------------------------------------------------------------------------
  // Classic DurableFieldDescriptor utilities (backward-compatible)
  // ---------------------------------------------------------------------------

  private def dartType(f: DurableFieldDescriptor): String = {
    val base = mapType(f.typeName)
    if (f.optional) s"$base?" else base
  }

  private def mapType(typeName: String): String = typeName match {
    case "String" => "String"
    case "Int" => "int"
    case "Long" => "int"
    case "Boolean" => "bool"
    case "Double" => "double"
    case s if s.startsWith("List[") =>
      val inner = s.substring(5, s.length - 1)
      s"List<${mapType(inner)}>"
    case other => throw RuntimeException(s"Unsupported type for Dart mapping: $other")
  }

  private def argExpr(f: DurableFieldDescriptor): String = {
    val access = s"data['${f.name}']"
    if (f.optional) {
      s"$access as ${mapType(f.typeName)}?"
    } else {
      f.typeName match {
        case "Int" | "Long" => s"($access as int?) ?? 0"
        case "Double" => s"($access as num?)?.toDouble() ?? 0.0"
        case "Boolean" => s"($access as bool?) ?? false"
        case "String" => s"($access as String?) ?? ''"
        case t if t.startsWith("List[") =>
          val inner = mapType(t.substring(5, t.length - 1))
          s"($access as List?)?.cast<$inner>() ?? []"
        case _ => access
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Utilities
  // ---------------------------------------------------------------------------

  private def camelCase(snakeStr: String): String = {
    val parts = snakeStr.split("_")
    parts.head + parts.tail.map(_.capitalize).mkString
  }

  private def snakeCase(name: String): String = {
    val first = name.charAt(0).toLower
    val rest = "\\p{Lu}".r.replaceAllIn(name.substring(1), m => s"_${m.group(0).toLowerCase}")
    s"$first$rest"
  }

  /** Convert a raw string to a valid Dart identifier (lowercase first char, no spaces/dashes). */
  private def dartIdentifier(raw: String): String = {
    val sanitized = raw.replace("-", "_").replace(" ", "_")
    if (sanitized.nonEmpty && sanitized.charAt(0).isUpper)
      sanitized.charAt(0).toLower.toString + sanitized.substring(1)
    else sanitized
  }

  private def isGenerated(file: File): Boolean = {
    if (!file.isDirectory && file.getName.endsWith(".dart")) {
      val s = Source.fromFile(file)
      try s.getLines().exists(_.contains(generatedComment))
      finally s.close()
    } else false
  }

  private def loadString(name: String): String = {
    val stream = getClass.getClassLoader.getResourceAsStream(name)
    if (stream == null) throw RuntimeException(s"Template not found: $name")
    Streamer(stream, new mutable.StringBuilder).sync().toString
  }
}
