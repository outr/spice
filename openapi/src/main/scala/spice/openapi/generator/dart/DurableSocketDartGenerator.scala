package spice.openapi.generator.dart

import fabric.Str as FabricStr
import fabric.define.{DefType, Definition}
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

/** Descriptor for a REST tool whose input/output types are captured from RW Definitions.
  * Input params and return type are derived automatically — zero manual declarations. */
case class TypedRestTool(name: String, inputDef: Definition, outputDef: Definition)

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
  /** Named Definition pairs for full type generation */
  defTypes: List[(String, Definition)] = Nil,
  /** When true: handler uses storedEvent / transient / ephemeral dispatch pattern */
  storedEventMode: Boolean = false,
  /** Transient event descriptors dispatched via durable channel but not persisted.
    * These need full field info for generating typed stubs and dispatch cases. */
  transientEventKinds: List[DurableEventDescriptor] = Nil,
  /** Client → Server event types (typed Definitions). The generator emits sender methods
    * with proper constructors and toJson() serialization. Replaces clientEventKinds. */
  clientEventDefs: List[(String, Definition)] = Nil,
  /** Manually maintained Dart files to include in the barrel export.
    * These files are NOT generated — they must already exist in the output directory.
    * Paths are relative to the model directory (e.g. "geo.dart", "point.dart"). */
  manualExports: List[String] = Nil
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
    val methods = config.clientEventDefs.flatMap { case (name, defn) =>
      defn.defType match {
        case o: DefType.Obj if o.map.nonEmpty =>
          val dartClass = dartName(name)
          val stripped = name.replaceAll("^Client", "")
          val methodName = stripped.head.toLower + stripped.tail
          val params = o.map.toList.map { case (fname, fDefn) =>
            val dartType = defTypeToDartType(fDefn)
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
        case _: DefType.Obj =>
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
      val returnTypeName = (tool.outputDef.defType, tool.outputDef.className) match {
        case (_: DefType.Obj, Some(cn)) => dartName(simpleName(cn))
        case _ => "Map<String, dynamic>"
      }
      val (params, argEntries) = tool.inputDef.defType match {
        case o: DefType.Obj if o.map.nonEmpty =>
          val ps = o.map.map { case (fname, fDefn) => s"${defTypeToDartType(fDefn)} $fname" }.mkString(", ")
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
    val toEmit = mutable.LinkedHashMap.empty[String, Definition]
    config.defTypes.foreach { case (name, defn) => collectTypes(name, defn, toEmit) }
    config.typedRestTools.foreach { tool =>
      tool.inputDef.defType match {
        case o: DefType.Obj => o.map.foreach { case (_, fDefn) => collectNestedType(fDefn, toEmit) }
        case _ =>
      }
      (tool.outputDef.defType, tool.outputDef.className) match {
        case (_: DefType.Obj, Some(cn)) =>
          collectTypes(simpleName(cn), tool.outputDef, toEmit)
        case _ =>
      }
    }

    // Build child→parent map from poly types (exclude simple enums — their values are enum constants, not class subtypes)
    val childToParent = mutable.Map.empty[String, String]
    toEmit.foreach { case (name, defn) =>
      defn.defType match {
        case p: DefType.Poly if !isSimpleEnum(p) =>
          p.values.foreach { case (key, subDefn) =>
            val sn = shortName(key, subDefn)
            if (!childToParent.contains(sn)) childToParent(sn) = name
          }
        case _ =>
      }
    }
    val polyTypes = toEmit.collect { case (name, defn) if defn.defType.isInstanceOf[DefType.Poly] =>
      val p = defn.defType.asInstanceOf[DefType.Poly]
      name -> p.values.map { case (key, subDefn) => shortName(key, subDefn) }.toSet
    }
    for ((childPolyName, childSubs) <- polyTypes; (parentPolyName, parentSubs) <- polyTypes if childPolyName != parentPolyName) {
      if (childSubs.nonEmpty && childSubs.subsetOf(parentSubs) && !childToParent.contains(childPolyName)) {
        childToParent(childPolyName) = parentPolyName
      }
    }

    // Track all generated file names for the barrel file
    val exportNames = mutable.ListBuffer.empty[String]

    // Generate typed wrappers first (they're referenced by other types)
    // Force wrapper discovery by doing a dry-run type scan
    toEmit.foreach { case (_, defn) => scanForWrappers(defn) }
    discoveredWrappers.foreach { case (_, (wrapperName, primitiveDart)) =>
      val fileName = s"${snakeCase(wrapperName)}.dart"
      val source = wrapTypedWrapper(wrapperName, primitiveDart)
      files += SourceFile("Dart", wrapperName, fileName, modelPath, source)
      exportNames += fileName
    }

    // Build type → subDir mapping from classNames
    toEmit.foreach { case (name, defn) =>
      val dn = dartName(name)
      val subDir = defn.className.map(packageSubDir).getOrElse("")
      typeLocations(dn) = (dn, subDir)
      // Also map poly subtypes
      defn.defType match {
        case p: DefType.Poly =>
          p.values.foreach { case (key, subDefn) =>
            val subName = dartName(shortName(key, subDefn))
            val subSubDir = subDefn.className.map(packageSubDir).getOrElse(subDir)
            typeLocations(subName) = (subName, subSubDir)
          }
        case _ =>
      }
    }

    /** Resolve an import path from one type's subDir to another type's file.
      * Supports multi-level package paths via standard relative-path computation. */
    def importPath(fromSubDir: String, targetTypeName: String): String = {
      val (_, targetSubDir) = typeLocations.getOrElse(targetTypeName, (targetTypeName, ""))
      val targetFile = s"${snakeCase(targetTypeName)}.dart"
      if (fromSubDir == targetSubDir) targetFile
      else {
        val fromParts = fromSubDir.split("/").toList.filter(_.nonEmpty)
        val toParts = targetSubDir.split("/").toList.filter(_.nonEmpty)
        val commonLen = fromParts.zip(toParts).takeWhile { case (a, b) => a == b }.length
        val ups = "../" * (fromParts.length - commonLen)
        val downs = toParts.drop(commonLen).mkString("/")
        if (downs.nonEmpty) s"$ups$downs/$targetFile" else s"$ups$targetFile"
      }
    }

    // Generate each type
    toEmit.foreach { case (name, defn) =>
      if (!dartReserved.contains(name)) {
        val dn = dartName(name)
        val fileName = s"${snakeCase(dn)}.dart"
        val (_, subDir) = typeLocations.getOrElse(dn, (dn, ""))
        val filePath = if (subDir.nonEmpty) s"$modelPath/$subDir" else modelPath
        val parent = childToParent.get(name).map(dartName)

        defn.defType match {
          case p: DefType.Poly if isSimpleEnum(p) =>
            files += SourceFile("Dart", dn, fileName, filePath, wrapEnum(dn, generateDartEnumFromPoly(dn, p)))
            exportNames += (if (subDir.nonEmpty) s"$subDir/$fileName" else fileName)
          case o: DefType.Obj =>
            val rawImports = collectImports(o.map.values.toList, dn, toEmit, childToParent)
            // Resolve each Dart class name to a package-relative import path
            val rewrittenImports = rawImports.map(t => s"import '${importPath(subDir, t)}';")
            val parentImport = parent.map(p => s"import '${importPath(subDir, p)}';").toList
            val allImports = (rewrittenImports ++ parentImport).distinct.sorted
            // Discriminator: full className (without type args) when known, otherwise the simple Dart name
            val discriminator = defn.className.map(baseClassName)
            files += SourceFile("Dart", dn, fileName, filePath,
              wrapClass(dn, fileName, generateDartClassBody(dn, o, toEmit, parent, discriminator), allImports, hasFields = o.map.nonEmpty))
            exportNames += (if (subDir.nonEmpty) s"$subDir/$fileName" else fileName)
          case p: DefType.Poly =>
            val subImports = p.values.map { case (key, subDefn) =>
              val subName = dartName(shortName(key, subDefn))
              s"import '${importPath(subDir, subName)}';"
            }.toList
            val parentImport = parent.map(p => s"import '${importPath(subDir, p)}';").toList
            val commonFieldImports = detectCommonFieldImports(p).map(t => s"import '${importPath(subDir, t)}';")
            val allImports = (subImports ++ parentImport ++ commonFieldImports).distinct.sorted
            files += SourceFile("Dart", dn, fileName, filePath,
              wrapPoly(dn, generateDartPoly(dn, p, toEmit, parent, defn.className), allImports))
            exportNames += (if (subDir.nonEmpty) s"$subDir/$fileName" else fileName)
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

    // Include manually maintained exports
    config.manualExports.foreach(f => exportNames += f)

    // Barrel file that re-exports everything (backwards-compatible import)
    val exports = exportNames.sorted.map(f => s"export 'model/$f';").mkString("\n")
    val barrelSource =
      s"""$generatedComment
         |$exports
         |""".stripMargin
    files += SourceFile("Dart", s"${sn}Types", s"${snakeCase(sn)}_types.dart", "lib/ws/durable", barrelSource)

    files.toList
  }

  /** Scan a Definition tree to discover typed wrappers (Id, Timestamp, etc.) without generating code. */
  private def scanForWrappers(d: Definition): Unit = {
    d.className.foreach { cn =>
      d.defType match {
        case DefType.Str | DefType.Int | DefType.Dec | DefType.Bool =>
          val baseCn = baseClassName(cn)
          val (baseName, _) = parseClassName(cn)
          val primitiveDart = defTypeToDartPrimitive(d.defType)
          discoveredWrappers.getOrElseUpdate(baseCn, (baseName, primitiveDart))
        case _ =>
      }
    }
    d.defType match {
      case DefType.Obj(m) => m.values.foreach(scanForWrappers)
      case DefType.Poly(v) => v.values.foreach(scanForWrappers)
      case DefType.Arr(inner) => scanForWrappers(inner)
      case DefType.Opt(inner) => scanForWrappers(inner)
      case _ =>
    }
  }

  /** Collect import statements for field types that reference other generated types. */
  /** Collect Dart class names that the given field types reference, excluding the self type.
    * Returns a sorted list of Dart class names (no `import` formatting). The caller resolves
    * paths via `importPath` to produce the final import statements. */
  private def collectImports(
    fieldTypes: List[Definition],
    selfName: String,
    knownTypes: mutable.LinkedHashMap[String, Definition],
    childToParent: mutable.Map[String, String]
  ): List[String] = {
    val names = mutable.Set.empty[String]
    def scan(d: Definition): Unit = {
      d.className.foreach { cn =>
        d.defType match {
          case DefType.Str | DefType.Int | DefType.Dec | DefType.Bool =>
            // Typed wrapper (e.g., UserId)
            val name = simpleName(cn)
            if (name != selfName) names += name
          case _: DefType.Obj =>
            val name = dartName(simpleName(cn))
            if (name != selfName) names += name
          case _: DefType.Poly if isSimpleEnum(d.defType.asInstanceOf[DefType.Poly]) =>
            val name = simpleName(cn)
            if (name != selfName && !dartReserved.contains(name)) names += name
          case _: DefType.Poly =>
            val name = dartName(simpleName(cn))
            if (name != selfName) names += name
          case _ =>
        }
      }
      d.defType match {
        case DefType.Arr(inner) => scan(inner)
        case DefType.Opt(inner) => scan(inner)
        case _ =>
      }
    }
    fieldTypes.foreach(scan)
    names.toList.sorted
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
    knownTypes: mutable.LinkedHashMap[String, Definition],
    parentName: Option[String] = None,
    discriminator: Option[String] = None
  ): String = generateDartClass(name, o, knownTypes, parentName, discriminator)

  /** Recursively collect named types from a Definition tree into `out` (in dependency order). */
  private def collectTypes(name: String, defn: Definition, out: mutable.LinkedHashMap[String, Definition]): Unit = {
    if (out.contains(name)) return
    defn.defType match {
      case o: DefType.Obj =>
        // Recurse into field types first
        o.map.foreach { case (_, fieldDefn) => collectNestedType(fieldDefn, out) }
        out(name) = defn
      case p: DefType.Poly =>
        // Only collect subtypes as separate classes if NOT a simple enum
        // (simple enums generate a Dart enum, not class hierarchy)
        if (!isSimpleEnum(p)) {
          p.values.foreach { case (subName, subDefn) => collectTypes(shortName(subName, subDefn), subDefn, out) }
        }
        out(name) = defn
      case DefType.Arr(inner) => collectNestedType(inner, out)
      case DefType.Opt(inner) => collectNestedType(inner, out)
      case _ => // primitives — no top-level type to register
    }
  }

  private def collectNestedType(defn: Definition, out: mutable.LinkedHashMap[String, Definition]): Unit = {
    val cn = defn.className
    defn.defType match {
      case _: DefType.Obj if cn.isDefined => collectTypes(cn.get.split('.').last, defn, out)
      case _: DefType.Poly if cn.isDefined => collectTypes(cn.get.split('.').last, defn, out)
      case DefType.Arr(inner) => collectNestedType(inner, out)
      case DefType.Opt(inner) => collectNestedType(inner, out)
      case _ => ()
    }
  }

  private def shortName(key: String, defn: Definition): String = {
    defn.className match {
      case Some(cn) =>
        val short = simpleName(cn)
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

  /** Detect if a Poly represents a simple enum (all variants have DefType.Null). */
  private def isSimpleEnum(p: DefType.Poly): Boolean =
    p.values.nonEmpty && p.values.values.forall(_.defType.isNull)

  /** Generate a Dart enum from a Poly that represents a simple enum. */
  private def generateDartEnumFromPoly(name: String, p: DefType.Poly): String = {
    val values = p.values.keys.map(dartIdentifier).mkString(",\n  ")
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
    knownTypes: mutable.LinkedHashMap[String, Definition],
    parentName: Option[String] = None,
    discriminator: Option[String] = None
  ): String = {
    val extendsClause = parentName.map(p => s" extends $p").getOrElse("")
    val typeValue = discriminator.getOrElse(name)
    if (o.map.isEmpty) {
      // Empty class (e.g., case object with no fields)
      s"""class $name$extendsClause {
         |  $name();
         |  $name.fromJson(Map<String, dynamic> json);
         |
         |  Map<String, dynamic> toJson() => {'type': '$typeValue'};
         |}""".stripMargin
    } else {
      val fields = o.map.map { case (fname, fDefn) =>
        val docComment = fDefn.description.map(d => s"  /// $d\n").getOrElse("")
        val deprecatedAnnotation = if (fDefn.deprecated) s"  @Deprecated('Deprecated field')\n" else ""
        s"$docComment$deprecatedAnnotation  final ${defTypeToDartType(fDefn)} ${dartFieldName(fname)};"
      }.mkString("\n")

      val ctorParams = o.map.toList.map { case (fname, fDefn) =>
        val dartField = dartFieldName(fname)
        val dartType = defTypeToDartType(fDefn)
        if (dartType.endsWith("?")) s"this.$dartField" else s"required this.$dartField"
      }.mkString(", ")

      val fromJsonInits = o.map.toList.map { case (fname, fDefn) =>
        s"${dartFieldName(fname)} = ${defTypeFromJson(fname, fDefn)}"
      }.mkString(",\n        ")

      val toJsonEntries = o.map.toList.map { case (fname, fDefn) =>
        s"'$fname': ${defTypeToJsonExpr(dartFieldName(fname), fDefn)}"
      }.mkString(", ")

      s"""class $name$extendsClause {
         |$fields
         |
         |  $name({$ctorParams});
         |
         |  $name.fromJson(Map<String, dynamic> json)
         |      : $fromJsonInits;
         |
         |  Map<String, dynamic> toJson() => {'type': '$typeValue', $toJsonEntries};
         |}""".stripMargin
    }
  }

  private def generateDartPoly(
    name: String,
    p: DefType.Poly,
    knownTypes: mutable.LinkedHashMap[String, Definition],
    parentName: Option[String] = None,
    parentFullClass: Option[String] = None
  ): String = {
    val extendsClause = parentName.map(p => s" extends $p").getOrElse("")
    val cases = p.values.map { case (key, defn) =>
      val subName = dartName(shortName(key, defn))
      // Use full className as discriminator when meaningful (not an anonymous class name).
      // For anonymous case objects in enums, derive from parent's full path + key.
      val discriminatorValue = defn.className match {
        case Some(cn) if !cn.contains("anon") => baseClassName(cn)
        case _ =>
          parentFullClass.map(p => s"${baseClassName(p)}.$key").getOrElse(key)
      }
      s"""    if (type == '$discriminatorValue') return $subName.fromJson(json);"""
    }.mkString("\n")

    // Detect common fields across all subtypes
    val subtypeFields: List[Map[String, (String, Boolean)]] = p.values.values.toList.flatMap { defn =>
      extractObjFields(defn).map(_.map { case (fieldName, fieldDefn, optional) =>
        fieldName -> (defTypeToDartType(fieldDefn), optional)
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

  /** Extract field definitions from a Definition if it's an Obj. */
  private def extractObjFields(defn: Definition): Option[List[(String, Definition, Boolean)]] = defn.defType match {
    case DefType.Obj(fields) =>
      Some(fields.map { case (name, fieldDefn) =>
        val optional = fieldDefn.defType match {
          case _: DefType.Opt => true
          case _ => false
        }
        val innerDefn = fieldDefn.defType match {
          case DefType.Opt(inner) => inner
          case _ => fieldDefn
        }
        (name, innerDefn, optional)
      }.toList)
    case _ => None
  }

  /** Detect which imports are needed for common fields in a poly type. */
  /** Collect Dart class names appearing as common fields across all subtypes of a Poly,
    * excluding primitives and collections. Caller resolves paths to produce import statements. */
  private def detectCommonFieldImports(p: DefType.Poly): List[String] = {
    val subtypeFields: List[Map[String, (String, Boolean)]] = p.values.values.toList.flatMap { defn =>
      extractObjFields(defn).map(_.map { case (fieldName, fieldDefn, optional) =>
        fieldName -> (defTypeToDartType(fieldDefn), optional)
      }.toMap)
    }
    if (subtypeFields.size < 2) return Nil
    val allFieldNames = subtypeFields.map(_.keySet)
    val commonNames = allFieldNames.reduce(_ intersect _) - "type"
    val commonTypes = commonNames.flatMap { fieldName =>
      val types = subtypeFields.flatMap(_.get(fieldName)).map(_._1).distinct
      if (types.size == 1) Some(types.head) else None
    }.toSet
    val primitives = Set("String", "int", "double", "bool", "num", "dynamic", "Object", "void", "List", "Map")
    commonTypes.filterNot(t => primitives.contains(t) || t.startsWith("List<") || t.startsWith("Map<")).toList
  }

  /** Track typed wrappers discovered during generation. Key = base className (no type params), Value = (dartName, primitiveDartType). */
  private val discoveredWrappers = scala.collection.mutable.LinkedHashMap.empty[String, (String, String)]

  /** Parse a className like "lightdb.id.Id[User]" into ("Id", Some(List("User"))).
    * Returns (baseName, typeArgs) where typeArgs is None if not parameterized. */
  private def parseClassName(cn: String): (String, Option[List[String]]) = {
    val dotIdx = cn.lastIndexOf('.')
    val short = if (dotIdx >= 0) cn.substring(dotIdx + 1) else cn
    val bracketIdx = short.indexOf('[')
    if (bracketIdx == -1) (short, None)
    else {
      val base = short.substring(0, bracketIdx)
      val argsStr = short.substring(bracketIdx + 1, short.lastIndexOf(']'))
      val args = argsStr.split(',').map(_.trim).toList
      (base, Some(args))
    }
  }

  /** Extract the base className (without type parameters) from a full className string.
    * "com.example.Id[User]" → "com.example.Id" */
  private def baseClassName(cn: String): String = {
    val bracketIdx = cn.indexOf('[')
    if (bracketIdx == -1) cn else cn.substring(0, bracketIdx)
  }

  /** Extract the simple name from a className, stripping package and type args.
    * "com.example.Id[User]" → "Id" */
  private def simpleName(cn: String): String = {
    val (base, _) = parseClassName(cn)
    base
  }

  /** Extract the full package path from a className as a slash-separated string.
    * Walks the className taking leading lowercase-first segments as package parts.
    * "scalagentic.conversation.event.Deleted" → "scalagentic/conversation/event"
    * "com.outr.workflow.step.StepResultStatus.Completed" → "com/outr/workflow/step"
    * "spec.OpenAPIHttpServerSpec.Auth" → "spec"
    * Names without packages get an empty string. */
  private def packageSubDir(cn: String): String = {
    val bare = baseClassName(cn).replace("$", ".")
    val parts = bare.split('.').toList.filter(_.nonEmpty)
    val pkgParts = parts.takeWhile(p => p.charAt(0).isLower)
    pkgParts.mkString("/")
  }

  /** Tracks className → (dartName, packagePath) for all emitted types.
    * Used to resolve cross-package import paths. */
  private val typeLocations = mutable.Map.empty[String, (String, String)]

  /** Map a bare DefType (no metadata) to a Dart primitive type name. */
  private def defTypeToDartPrimitive(dt: DefType): String = dt match {
    case DefType.Str => "String"
    case DefType.Int => "int"
    case DefType.Dec => "double"
    case DefType.Bool => "bool"
    case DefType.Json => "Map<String, dynamic>"
    case DefType.Null => "Null"
    case _ => "dynamic"
  }

  private def defTypeToDartType(d: Definition): String = {
    // Format.DateTime on a string field → Dart DateTime
    if (d.format == fabric.define.Format.DateTime && d.defType == DefType.Str && d.className.isEmpty) {
      return "DateTime"
    }
    d.className match {
      case Some(cn) =>
        d.defType match {
          case DefType.Str | DefType.Int | DefType.Dec | DefType.Bool =>
            val baseCn = baseClassName(cn)
            val (baseName, typeArgs) = parseClassName(cn)
            val primitiveDart = defTypeToDartPrimitive(d.defType)
            discoveredWrappers.getOrElseUpdate(baseCn, (baseName, primitiveDart))
            typeArgs match {
              case Some(args) => s"$baseName<${args.mkString(", ")}>"
              case None => baseName
            }
          case p: DefType.Poly if isSimpleEnum(p) =>
            val (baseName, _) = parseClassName(cn)
            if (dartReserved.contains(baseName)) "String" else baseName
          case _: DefType.Obj =>
            val (baseName, _) = parseClassName(cn)
            dartName(baseName)
          case _: DefType.Poly =>
            val (baseName, _) = parseClassName(cn)
            dartName(baseName)
          case _ => defTypeToDartTypeInner(d)
        }
      case None => defTypeToDartTypeInner(d)
    }
  }

  private def defTypeToDartTypeInner(d: Definition): String = d.defType match {
    case DefType.Str => "String"
    case DefType.Int => "int"
    case DefType.Dec => "double"
    case DefType.Bool => "bool"
    case DefType.Json => "Map<String, dynamic>"
    case DefType.Null => "Null"
    case DefType.Arr(inner) => s"List<${defTypeToDartType(inner)}>"
    case DefType.Opt(inner) => s"${defTypeToDartType(inner)}?"
    case p: DefType.Poly if isSimpleEnum(p) => "String"
    case _: DefType.Obj => "Map<String, dynamic>"
    case _: DefType.Poly => "dynamic"
  }

  /** Generate a Dart expression that deserializes a field from a Map<String, dynamic>. */
  private def defTypeFromJson(fieldName: String, d: Definition): String = {
    val access = s"json['$fieldName']"
    defTypeFromJsonExpr(access, d)
  }

  private def defTypeFromJsonExpr(access: String, d: Definition): String = {
    // Format.DateTime: parse ISO-8601 string into DateTime
    if (d.format == fabric.define.Format.DateTime && d.defType == DefType.Str && d.className.isEmpty) {
      return s"DateTime.parse($access as String)"
    }
    d.className match {
      case Some(cn) =>
        d.defType match {
          case DefType.Str | DefType.Int | DefType.Dec | DefType.Bool =>
            val (baseName, _) = parseClassName(cn)
            val primitiveExpr = defTypeFromJsonExprInner(access, Definition(d.defType))
            return s"$baseName($primitiveExpr)"
          case _ =>
        }
      case None =>
    }
    defTypeFromJsonExprInner(access, d)
  }

  private def defTypeFromJsonExprInner(access: String, d: Definition): String = d.defType match {
    case DefType.Str => s"$access as String? ?? ''"
    case DefType.Int => s"($access as int?) ?? 0"
    case DefType.Dec => s"($access as num?)?.toDouble() ?? 0.0"
    case DefType.Bool => s"($access as bool?) ?? false"
    case DefType.Json => s"$access is Map<String, dynamic> ? $access as Map<String, dynamic> : <String, dynamic>{}"
    case DefType.Null => "null"
    case DefType.Opt(inner) if inner.className.isDefined && isPrimitive(inner.defType) =>
      val (baseName, _) = parseClassName(inner.className.get)
      s"$access != null ? $baseName(${defTypeFromJsonExprInner(access, Definition(inner.defType))}) : null"
    case DefType.Opt(inner) =>
      val innerDart = defTypeToDartType(inner)
      inner.defType match {
        case DefType.Str => s"$access as String?"
        case DefType.Int => s"$access as int?"
        case DefType.Dec => s"($access as num?)?.toDouble()"
        case DefType.Bool => s"$access as bool?"
        case p: DefType.Poly if isSimpleEnum(p) =>
          inner.className match {
            case Some(cn) =>
              val sub = simpleName(cn)
              if (dartReserved.contains(sub)) s"$access as String?"
              else s"$sub.fromString($access as String?)"
            case None => s"$access as String?"
          }
        case _: DefType.Obj =>
          inner.className match {
            case Some(cn) => s"$access != null ? ${dartName(simpleName(cn))}.fromJson($access as Map<String, dynamic>) : null"
            case None => s"$access as $innerDart?"
          }
        case _: DefType.Poly =>
          inner.className match {
            case Some(cn) => s"$access != null ? ${dartName(simpleName(cn))}.fromJson($access as Map<String, dynamic>) : null"
            case None => s"$access as $innerDart?"
          }
        case DefType.Arr(_) =>
          val arrExpr = defTypeFromJsonExprInner(access, Definition(inner.defType))
          s"$access != null ? $arrExpr : null"
        case _ => s"$access as $innerDart?"
      }
    case DefType.Arr(inner) if inner.className.isDefined && isPrimitive(inner.defType) =>
      val (wrapperName, _) = parseClassName(inner.className.get)
      val primitiveJsonType = inner.defType match {
        case DefType.Str => "String"
        case DefType.Int => "int"
        case DefType.Dec => "double"
        case DefType.Bool => "bool"
        case _ => "dynamic"
      }
      s"($access as List?)?.map((e) => $wrapperName(e as $primitiveJsonType)).toList() ?? []"
    case DefType.Arr(inner) =>
      val innerDart = defTypeToDartType(inner)
      inner.defType match {
        case DefType.Str | DefType.Int | DefType.Bool | DefType.Dec =>
          s"($access as List?)?.cast<$innerDart>() ?? []"
        case _: DefType.Obj =>
          inner.className match {
            case Some(cn) =>
              val sub = dartName(simpleName(cn))
              s"($access as List?)?.map((e) => $sub.fromJson(e as Map<String, dynamic>)).toList() ?? []"
            case None =>
              s"($access as List?)?.cast<$innerDart>() ?? []"
          }
        case _: DefType.Poly =>
          inner.className match {
            case Some(cn) =>
              val sub = dartName(simpleName(cn))
              s"($access as List?)?.map((e) => $sub.fromJson(e as Map<String, dynamic>)).toList() ?? []"
            case None =>
              s"($access as List?)?.cast<$innerDart>() ?? []"
          }
        case _: DefType.Arr =>
          s"($access as List?)?.cast<$innerDart>() ?? []"
        case _ =>
          s"($access as List?)?.cast<$innerDart>() ?? []"
      }
    case p: DefType.Poly if isSimpleEnum(p) =>
      d.className match {
        case Some(cn) =>
          val sub = simpleName(cn)
          if (dartReserved.contains(sub)) s"$access as String? ?? ''"
          else s"$sub.fromString($access as String?) ?? $sub.values.first"
        case None => s"$access as String? ?? ''"
      }
    case _: DefType.Obj =>
      d.className match {
        case Some(cn) =>
          val sub = dartName(simpleName(cn))
          s"$sub.fromJson($access as Map<String, dynamic>)"
        case None => s"$access as dynamic"
      }
    case _: DefType.Poly =>
      d.className match {
        case Some(cn) =>
          val sub = dartName(simpleName(cn))
          s"$sub.fromJson($access as Map<String, dynamic>)"
        case None => s"$access as dynamic"
      }
    case _ => s"$access as dynamic"
  }

  private def isPrimitive(dt: DefType): Boolean = dt match {
    case DefType.Str | DefType.Int | DefType.Dec | DefType.Bool => true
    case _ => false
  }

  /** Generate a Dart expression that serializes a field value to JSON-compatible form. */
  private def defTypeToJsonExpr(access: String, d: Definition): String = {
    // Format.DateTime: serialize DateTime as ISO-8601 string
    if (d.format == fabric.define.Format.DateTime && d.defType == DefType.Str && d.className.isEmpty) {
      return s"$access.toIso8601String()"
    }
    // Handle typed wrappers (className + primitive)
    d.className match {
      case Some(_) if isPrimitive(d.defType) => return s"$access.value"
      case _ =>
    }
    d.defType match {
      case DefType.Str | DefType.Int | DefType.Dec | DefType.Bool | DefType.Json | DefType.Null => access
      case DefType.Opt(inner) => s"$access != null ? ${defTypeToJsonExpr(s"$access!", inner)} : null"
      case DefType.Arr(inner) =>
        if (inner.className.isDefined && isPrimitive(inner.defType)) {
          s"$access.map((e) => e.value).toList()"
        } else inner.defType match {
          case DefType.Str | DefType.Int | DefType.Bool | DefType.Dec => access
          case _: DefType.Obj if inner.className.isDefined => s"$access.map((e) => e.toJson()).toList()"
          case _: DefType.Poly if inner.className.isDefined => s"$access.map((e) => e.toJson()).toList()"
          case _ => access
        }
      case p: DefType.Poly if isSimpleEnum(p) =>
        d.className match {
          case Some(cn) =>
            val sub = simpleName(cn)
            if (dartReserved.contains(sub)) access else s"$access.name"
          case None => access
        }
      case _: DefType.Obj if d.className.isDefined => s"$access.toJson()"
      case _: DefType.Poly if d.className.isDefined => s"$access.toJson()"
      case _ => access
    }
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
