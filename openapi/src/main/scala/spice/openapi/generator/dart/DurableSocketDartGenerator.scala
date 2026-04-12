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

/** Descriptor for a REST tool invocation with manual param declarations. */
case class RestToolDescriptor(name: String, params: List[(String, String)])

/** Descriptor for a REST tool whose input/output types are captured from RW DefTypes.
  * Input params and return type are derived automatically — zero manual declarations. */
case class TypedRestTool(name: String, inputDef: DefType, outputDef: DefType)

/** Configuration for the DurableSocket Dart code generator.
  * Event descriptors come from DurableEventIntrospect — no manual lists. */
case class DurableSocketDartConfig(
  serviceName: String,
  /** Server → Client durable event kinds (from introspection) */
  eventKinds: List[DurableEventDescriptor] = Nil,
  /** Server → Client ephemeral kinds (from introspection) */
  ephemeralKinds: List[DurableEventDescriptor] = Nil,
  /** @deprecated Use clientEventDefs instead */
  clientEventKinds: List[DurableEventDescriptor] = Nil,
  /** REST tools (via POST /api/invoke) — manual descriptors */
  restTools: List[RestToolDescriptor] = Nil,
  /** REST tools with typed DefType input/output — auto-derived params and return types */
  typedRestTools: List[TypedRestTool] = Nil,
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
  transientEventKinds: List[DurableEventDescriptor] = Nil,
  /** Client → Server event types (typed DefTypes). The generator emits sender methods
    * with proper constructors and toJson() serialization. Replaces clientEventKinds. */
  clientEventDefs: List[(String, DefType)] = Nil
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

  /** Rename fields that would be private in Dart (underscore prefix). */
  private def dartFieldName(name: String): String = if (name.startsWith("_")) name.drop(1) else name

  def generate(): List[SourceFile] = {
    val base = List(generateClient(), generateHandler(), generateSender(), generateRestClient())
    if (config.defTypes.nonEmpty || config.enums.nonEmpty || config.typedRestTools.nonEmpty)
      base ++ generateDefTypeFiles()
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
    // Typed methods from clientEventDefs — uses generated class constructors + toJson()
    val methods = config.clientEventDefs.flatMap { case (name, dt) =>
      dt match {
        case o: DefType.Obj if o.map.nonEmpty =>
          val dartClass = dartName(name)
          val stripped = name.replaceAll("^Client", "")
          val methodName = stripped.head.toLower + stripped.tail
          val params = o.map.toList.map { case (fname, fdt) =>
            val dartType = defTypeToDartType(fdt)
            val dartField = dartFieldName(fname)
            if (dartType.endsWith("?")) s"$dartType $dartField" else s"required $dartType $dartField"
          }.mkString(", ")
          val args = o.map.toList.map { case (fname, _) =>
            s"${dartFieldName(fname)}: ${dartFieldName(fname)}"
          }.mkString(", ")
          Some(
            s"""  void $methodName({$params}) {
               |    push($dartClass($args).toJson());
               |  }""".stripMargin)
        case o: DefType.Obj =>
          // Empty class (no fields)
          val dartClass = dartName(name)
          val stripped = name.replaceAll("^Client", "")
          val methodName = stripped.head.toLower + stripped.tail
          Some(
            s"""  void $methodName() {
               |    push($dartClass().toJson());
               |  }""".stripMargin)
        case _ => None
      }
    }

    val allMethods = methods.mkString("\n\n")
    val typeImports = if (config.clientEventDefs.nonEmpty) {
      s"import '${snakeCase(sn)}_types.dart';\n"
    } else ""

    val source = SenderTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%SERVICE_SNAKE%%", snakeCase(sn))
      .replace("%%TYPE_IMPORTS%%", typeImports)
      .replace("%%METHODS%%", allMethods)

    SourceFile("Dart", s"${sn}DurableSender", s"${snakeCase(sn)}_durable_sender.dart", "lib/ws/durable", source)
  }

  // ---------------------------------------------------------------------------
  // REST client
  // ---------------------------------------------------------------------------

  private def generateRestClient(): SourceFile = {
    val manualMethods = config.restTools.map { tool =>
      val params = tool.params.map { case (n, t) => s"$t $n" }.mkString(", ")
      val argEntries = tool.params.map { case (n, _) => s"'$n': $n" }.mkString(", ")
      s"""  Future<Map<String, dynamic>> ${camelCase(tool.name)}($params) {
         |    return invoke('${tool.name}', {$argEntries});
         |  }""".stripMargin
    }

    val typedMethods = config.typedRestTools.map { tool =>
      val returnTypeName = tool.outputDef match {
        case DefType.Obj(_, Some(cn), _) => dartName(cn.split('.').last)
        case _ => "Map<String, dynamic>"
      }
      val (params, argEntries) = tool.inputDef match {
        case o: DefType.Obj if o.map.nonEmpty =>
          val ps = o.map.map { case (fname, fdt) => s"${defTypeToDartType(fdt)} $fname" }.mkString(", ")
          val args = o.map.keys.map(fname => s"'$fname': $fname").mkString(", ")
          (ps, args)
        case _ => ("", "")
      }
      if (returnTypeName == "Map<String, dynamic>") {
        s"""  Future<Map<String, dynamic>> ${camelCase(tool.name)}($params) {
           |    return invoke('${tool.name}', {$argEntries});
           |  }""".stripMargin
      } else {
        s"""  Future<$returnTypeName> ${camelCase(tool.name)}($params) async {
           |    final json = await invoke('${tool.name}', {$argEntries});
           |    return $returnTypeName.fromJson(json);
           |  }""".stripMargin
      }
    }

    val allMethods = (manualMethods ++ typedMethods).mkString("\n\n")
    val typesImport = s"${snakeCase(sn)}_types.dart"

    val source = RestTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%TYPES_IMPORT%%", typesImport)
      .replace("%%METHODS%%", allMethods)

    SourceFile("Dart", s"${sn}RestClient", s"${snakeCase(sn)}_rest_client.dart", "lib/ws/durable", source)
  }

  // ---------------------------------------------------------------------------
  // DefType-based type generation
  // ---------------------------------------------------------------------------

  /** Output path for generated model files (relative to project root). */
  private val modelPath = "lib/ws/durable/model"

  /** Generate one SourceFile per type definition, plus a barrel file for re-exports. */
  def generateDefTypeFiles(): List[SourceFile] = {
    val files = mutable.ListBuffer.empty[SourceFile]

    // Collect all types that need to be emitted, recursively
    val toEmit = mutable.LinkedHashMap.empty[String, DefType]
    config.defTypes.foreach { case (name, dt) => collectTypes(name, dt, toEmit) }
    config.typedRestTools.foreach { tool =>
      tool.inputDef match {
        case o: DefType.Obj => o.map.foreach { case (_, fdt) => collectNestedType(fdt, toEmit) }
        case _ =>
      }
      tool.outputDef match {
        case o: DefType.Obj if o.className.isDefined =>
          collectTypes(o.className.get.split('.').last, tool.outputDef, toEmit)
        case _ =>
      }
    }

    // Build child→parent map from poly types
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
    val polyTypes = toEmit.collect { case (name, p: DefType.Poly) => name -> p.values.map { case (key, dt) => shortName(key, dt) }.toSet }
    for ((childPolyName, childSubs) <- polyTypes; (parentPolyName, parentSubs) <- polyTypes if childPolyName != parentPolyName) {
      if (childSubs.nonEmpty && childSubs.subsetOf(parentSubs) && !childToParent.contains(childPolyName)) {
        childToParent(childPolyName) = parentPolyName
      }
    }

    // Track all generated file names for the barrel file
    val exportNames = mutable.ListBuffer.empty[String]

    // Generate typed wrappers first (they're referenced by other types)
    // Force wrapper discovery by doing a dry-run type scan
    toEmit.foreach { case (_, dt) => scanForWrappers(dt) }
    discoveredWrappers.foreach { case (_, (wrapperName, primitiveDart)) =>
      val fileName = s"${snakeCase(wrapperName)}.dart"
      val source = wrapTypedWrapper(wrapperName, primitiveDart)
      files += SourceFile("Dart", wrapperName, fileName, modelPath, source)
      exportNames += fileName
    }

    // Generate each type
    toEmit.foreach { case (name, dt) =>
      if (!dartReserved.contains(name)) {
        val dn = dartName(name)
        val fileName = s"${snakeCase(dn)}.dart"
        val parent = childToParent.get(name).map(dartName)

        dt match {
          case e: DefType.Enum =>
            files += SourceFile("Dart", dn, fileName, modelPath, wrapEnum(dn, generateDartEnum(dn, e)))
            exportNames += fileName
          case o: DefType.Obj =>
            val imports = collectImports(o.map.values.toList, dn, toEmit, childToParent)
            val parentImport = parent.map(p => s"import '${snakeCase(p)}.dart';").toList
            val allImports = (imports ++ parentImport).distinct.sorted
            files += SourceFile("Dart", dn, fileName, modelPath,
              wrapClass(dn, fileName, generateDartClassBody(dn, o, toEmit, parent), allImports, hasFields = o.map.nonEmpty))
            exportNames += fileName
          case p: DefType.Poly =>
            val subImports = p.values.map { case (key, subDt) =>
              val subName = dartName(shortName(key, subDt))
              s"import '${snakeCase(subName)}.dart';"
            }.toList
            val parentImport = parent.map(p => s"import '${snakeCase(p)}.dart';").toList
            // Detect common field types that need imports
            val commonFieldImports = detectCommonFieldImports(p)
            val allImports = (subImports ++ parentImport ++ commonFieldImports).distinct.sorted
            files += SourceFile("Dart", dn, fileName, modelPath,
              wrapPoly(dn, generateDartPoly(dn, p, toEmit, parent), allImports))
            exportNames += fileName
          case _ =>
        }
      }
    }

    // Simple DurableEnumDescriptor fallback enums
    config.enums.foreach { ed =>
      val fileName = s"${snakeCase(ed.name)}.dart"
      files += SourceFile("Dart", ed.name, fileName, modelPath, wrapEnum(ed.name, generateSimpleDartEnum(ed.name, ed.values)))
      exportNames += fileName
    }

    // Barrel file that re-exports everything (backwards-compatible import)
    val exports = exportNames.sorted.map(f => s"export 'model/$f';").mkString("\n")
    val barrelSource =
      s"""$generatedComment
         |$exports
         |""".stripMargin
    files += SourceFile("Dart", s"${sn}Types", s"${snakeCase(sn)}_types.dart", "lib/ws/durable", barrelSource)

    files.toList
  }

  /** Scan a DefType tree to discover typed wrappers (Id, Timestamp, etc.) without generating code. */
  private def scanForWrappers(dt: DefType): Unit = dt match {
    case DefType.Described(inner, _) => scanForWrappers(inner)
    case DefType.Classed(inner, cn) =>
      val name = cn.split('.').last
      val primitiveDart = defTypeToDartType(inner)
      discoveredWrappers.getOrElseUpdate(cn, (name, primitiveDart))
    case DefType.Obj(m, _, _) => m.values.foreach(scanForWrappers)
    case DefType.Poly(v, _, _) => v.values.foreach(scanForWrappers)
    case DefType.Arr(inner, _) => scanForWrappers(inner)
    case DefType.Opt(inner, _) => scanForWrappers(inner)
    case _ =>
  }

  /** Collect import statements for field types that reference other generated types. */
  private def collectImports(
    fieldTypes: List[DefType],
    selfName: String,
    knownTypes: mutable.LinkedHashMap[String, DefType],
    childToParent: mutable.Map[String, String]
  ): List[String] = {
    val imports = mutable.Set.empty[String]
    def scan(dt: DefType): Unit = dt match {
      case DefType.Described(inner, _) => scan(inner)
      case DefType.Classed(_, cn) =>
        val name = cn.split('.').last
        if (name != selfName) imports += s"import '${snakeCase(name)}.dart';"
      case DefType.Obj(_, Some(cn), _) =>
        val name = dartName(cn.split('.').last)
        if (name != selfName) imports += s"import '${snakeCase(name)}.dart';"
      case DefType.Poly(_, Some(cn), _) =>
        val name = dartName(cn.split('.').last)
        if (name != selfName) imports += s"import '${snakeCase(name)}.dart';"
      case DefType.Enum(_, Some(cn), _) =>
        val name = cn.split('.').last
        if (name != selfName && !dartReserved.contains(name)) imports += s"import '${snakeCase(name)}.dart';"
      case DefType.Arr(inner, _) => scan(inner)
      case DefType.Opt(inner, _) => scan(inner)
      case _ =>
    }
    fieldTypes.foreach(scan)
    imports.toList.sorted
  }

  /** Wrap a typed wrapper class in a standalone file. */
  private def wrapTypedWrapper(name: String, primitiveDart: String): String = {
    s"""$generatedComment
       |
       |${generateTypedWrapper(name, primitiveDart)}
       |""".stripMargin
  }

  /** Wrap an enum in a standalone file. */
  private def wrapEnum(name: String, body: String): String = {
    s"""$generatedComment
       |
       |$body
       |""".stripMargin
  }

  /** Wrap a concrete class in a standalone file. @CopyWith only for classes with fields. */
  private def wrapClass(name: String, fileName: String, body: String, imports: List[String], hasFields: Boolean): String = {
    val importsStr = if (imports.nonEmpty) imports.mkString("\n") + "\n" else ""
    if (hasFields) {
      val partFile = fileName.replace(".dart", ".g.dart")
      s"""$generatedComment
         |import 'package:copy_with_extension/copy_with_extension.dart';
         |
         |${importsStr}part '$partFile';
         |
         |@CopyWith()
         |$body
         |""".stripMargin
    } else {
      s"""$generatedComment
         |${importsStr}
         |$body
         |""".stripMargin
    }
  }

  /** Wrap a poly abstract class in a standalone file (no @CopyWith — abstract). */
  private def wrapPoly(name: String, body: String, imports: List[String]): String = {
    val importsStr = if (imports.nonEmpty) imports.mkString("\n") + "\n" else ""
    s"""$generatedComment
       |${importsStr}
       |$body
       |""".stripMargin
  }

  /** Generate the class body (without file wrapper) for a concrete class. */
  private def generateDartClassBody(
    name: String,
    o: DefType.Obj,
    knownTypes: mutable.LinkedHashMap[String, DefType],
    parentName: Option[String] = None
  ): String = generateDartClass(name, o, knownTypes, parentName)

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

  /** Generate a typed wrapper class for a Classed primitive (e.g., Id wrapping String). */
  private def generateTypedWrapper(name: String, primitiveDart: String): String = {
    s"""class $name {
       |  final $primitiveDart value;
       |  const $name(this.value);
       |
       |  @override
       |  String toString() => value.toString();
       |
       |  @override
       |  bool operator ==(Object other) => other is $name && other.value == value;
       |
       |  @override
       |  int get hashCode => value.hashCode;
       |}""".stripMargin
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
         |  $name();
         |  $name.fromJson(Map<String, dynamic> json);
         |
         |  Map<String, dynamic> toJson() => {'type': '$name'};
         |}""".stripMargin
    } else {
      val fields = o.map.map { case (fname, fdt) =>
        s"  final ${defTypeToDartType(fdt)} ${dartFieldName(fname)};"
      }.mkString("\n")

      val ctorParams = o.map.toList.map { case (fname, fdt) =>
        val dartField = dartFieldName(fname)
        val dartType = defTypeToDartType(fdt)
        if (dartType.endsWith("?")) s"this.$dartField" else s"required this.$dartField"
      }.mkString(", ")

      val fromJsonInits = o.map.toList.map { case (fname, fdt) =>
        s"${dartFieldName(fname)} = ${defTypeFromJson(fname, fdt)}"
      }.mkString(",\n        ")

      val toJsonEntries = o.map.toList.map { case (fname, fdt) =>
        s"'$fname': ${defTypeToJsonExpr(dartFieldName(fname), fdt)}"
      }.mkString(", ")

      s"""class $name$extendsClause {
         |$fields
         |
         |  $name({$ctorParams});
         |
         |  $name.fromJson(Map<String, dynamic> json)
         |      : $fromJsonInits;
         |
         |  Map<String, dynamic> toJson() => {'type': '$name', $toJsonEntries};
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

    // Detect common fields across all subtypes
    val subtypeFields: List[Map[String, (String, Boolean)]] = p.values.values.toList.flatMap { dt =>
      extractObjFields(dt).map(_.map { case (fieldName, fieldDt, optional) =>
        fieldName -> (defTypeToDartType(fieldDt), optional)
      }.toMap)
    }
    val commonFields = if (subtypeFields.size >= 2) {
      val allFieldNames = subtypeFields.map(_.keySet)
      val commonNames = allFieldNames.reduce(_ intersect _) - "type"
      commonNames.flatMap { fieldName =>
        val types = subtypeFields.flatMap(_.get(fieldName)).map(_._1).distinct
        if (types.size == 1) Some((fieldName, types.head)) else None
      }.toList.sortBy(_._1)
    } else Nil

    val commonGetters = commonFields.map { case (fieldName, dartType) =>
      s"  $dartType? get $fieldName;"
    }.mkString("\n")
    val getterBlock = if (commonGetters.nonEmpty) s"\n$commonGetters\n" else ""

    s"""abstract class $name$extendsClause {
       |  const $name();
       |$getterBlock
       |  Map<String, dynamic> toJson();
       |
       |  static $name fromJson(Map<String, dynamic> json) {
       |    final type = json['type'] as String?;
       |$cases
       |    throw ArgumentError('Unknown $name type: $$type (keys: $${json.keys.join(", ")})');
       |  }
       |}""".stripMargin
  }

  // ---------------------------------------------------------------------------
  // DefType → Dart type name
  // ---------------------------------------------------------------------------

  /** Extract field definitions from a DefType if it's an Obj (possibly wrapped in Described/Classed). */
  private def extractObjFields(dt: DefType): Option[List[(String, DefType, Boolean)]] = dt match {
    case DefType.Obj(fields, _, _) =>
      Some(fields.map { case (name, fieldDt) =>
        val optional = fieldDt match {
          case DefType.Opt(_, _) => true
          case _ => false
        }
        val innerDt = fieldDt match {
          case DefType.Opt(inner, _) => inner
          case other => other
        }
        (name, innerDt, optional)
      }.toList)
    case DefType.Described(inner, _) => extractObjFields(inner)
    case DefType.Classed(inner, _) => extractObjFields(inner)
    case _ => None
  }

  /** Detect which imports are needed for common fields in a poly type. */
  private def detectCommonFieldImports(p: DefType.Poly): List[String] = {
    val subtypeFields: List[Map[String, (String, Boolean)]] = p.values.values.toList.flatMap { dt =>
      extractObjFields(dt).map(_.map { case (fieldName, fieldDt, optional) =>
        fieldName -> (defTypeToDartType(fieldDt), optional)
      }.toMap)
    }
    if (subtypeFields.size < 2) return Nil
    val allFieldNames = subtypeFields.map(_.keySet)
    val commonNames = allFieldNames.reduce(_ intersect _) - "type"
    val commonTypes = commonNames.flatMap { fieldName =>
      val types = subtypeFields.flatMap(_.get(fieldName)).map(_._1).distinct
      if (types.size == 1) Some(types.head) else None
    }.toSet
    // Map non-primitive dart types to their import files
    val primitives = Set("String", "int", "double", "bool", "num", "dynamic", "Object", "void", "List", "Map")
    commonTypes.filterNot(t => primitives.contains(t) || t.startsWith("List<") || t.startsWith("Map<")).flatMap { dartType =>
      val snaked = snakeCase(dartType)
      Some(s"import '$snaked.dart';")
    }.toList
  }

  /** Track Classed wrappers discovered during generation. Key = className, Value = (dartName, primitiveDartType). */
  private val discoveredWrappers = scala.collection.mutable.LinkedHashMap.empty[String, (String, String)]

  private def defTypeToDartType(dt: DefType): String = dt match {
    case DefType.Described(inner, _) => defTypeToDartType(inner)
    case DefType.Classed(inner, cn) =>
      val dartName = cn.split('.').last
      val primitiveDart = defTypeToDartType(inner)
      discoveredWrappers.getOrElseUpdate(cn, (dartName, primitiveDart))
      dartName
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
    case DefType.Classed(inner, cn) =>
      val dartName = cn.split('.').last
      val primitiveExpr = defTypeFromJsonExpr(access, inner)
      s"$dartName($primitiveExpr)"
    case DefType.Str => s"$access as String? ?? ''"
    case DefType.Int => s"($access as int?) ?? 0"
    case DefType.Dec => s"($access as num?)?.toDouble() ?? 0.0"
    case DefType.Bool => s"($access as bool?) ?? false"
    case DefType.Json => s"$access is Map<String, dynamic> ? $access as Map<String, dynamic> : <String, dynamic>{}"
    case DefType.Null => "null"
    case DefType.Opt(DefType.Classed(unwrapped, cn), _) =>
      val dartName = cn.split('.').last
      // Use the raw access for null check, then wrap in constructor
      s"$access != null ? $dartName(${defTypeFromJsonExpr(access, unwrapped)}) : null"
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
    case DefType.Arr(DefType.Classed(unwrapped, cn), desc) =>
      val wrapperName = cn.split('.').last
      val primitiveDart = defTypeToDartType(unwrapped)
      val primitiveJsonType = unwrapped match {
        case DefType.Str => "String"
        case DefType.Int => "int"
        case DefType.Dec => "double"
        case DefType.Bool => "bool"
        case _ => "dynamic"
      }
      s"($access as List?)?.map((e) => $wrapperName(e as $primitiveJsonType)).toList() ?? []"
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

  /** Generate a Dart expression that serializes a field value to JSON-compatible form. */
  private def defTypeToJsonExpr(access: String, dt: DefType): String = dt match {
    case DefType.Described(inner, _) => defTypeToJsonExpr(access, inner)
    case DefType.Classed(_, _) => s"$access.value"
    case DefType.Str | DefType.Int | DefType.Dec | DefType.Bool | DefType.Json | DefType.Null => access
    case DefType.Opt(inner, _) => s"$access != null ? ${defTypeToJsonExpr(s"$access!", inner)} : null"
    case DefType.Arr(inner, _) => inner match {
      case DefType.Str | DefType.Int | DefType.Bool | DefType.Dec => access
      case DefType.Classed(_, _) => s"$access.map((e) => e.value).toList()"
      case DefType.Obj(_, Some(_), _) | DefType.Poly(_, Some(_), _) =>
        s"$access.map((e) => e.toJson()).toList()"
      case _ => access
    }
    case DefType.Enum(_, Some(cn), _) =>
      val sub = cn.split('.').last
      if (dartReserved.contains(sub)) access else s"$access.name"
    case DefType.Obj(_, Some(_), _) | DefType.Poly(_, Some(_), _) => s"$access.toJson()"
    case _ => access
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
