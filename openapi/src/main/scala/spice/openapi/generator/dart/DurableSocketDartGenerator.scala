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
  manualExports: List[String] = Nil,
  /** Wire type for the client↔server protocol. The generated client emits:
    *   - `int push(<Type> msg)` that internally calls `msg.toJson()`
    *   - `Stream<(int, <Type>)> get on<Type>` that maps inbound JSON via `<Type>.fromJson`
    * The Definition is auto-included in the generated types file — no need to also
    * list it in `defTypes`. */
  wireType: (String, Definition),
  /** Set of wire-type subtype discriminator values (Fabric `Product.productPrefix`
    * for case classes, the case name for Scala 3 enum cases) whose `push` should
    * be framed via the durable channel (`type: 'event'`, sequenced, replayable).
    * Subtypes NOT in this set are framed via the ephemeral channel (no seq, no
    * replay) — appropriate for transient pulses (Notices) and target-mutating
    * updates (Deltas) that the server-side `protocol.onEvent` handler is not
    * configured to decode.
    *
    * Default `Set.empty` preserves the legacy behavior of unconditionally framing
    * everything as durable. Apps with a mixed-kind wire vocabulary
    * (Event + Notice + Delta) populate this with the names of their durable
    * Event subtypes so `client.push(notice)` is framed correctly without the
    * consumer needing to know wire-format details. */
  durableSubtypes: Set[String] = Set.empty
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

  // Naming helpers delegate to `DartNames` — the single source of truth shared with
  // `OpenAPIDartGenerator`. Both generators MUST go through `DartNames` so the rules
  // (class-chain Dart names, leaf-only wire discriminator, FQN-mirroring file paths) cannot
  // drift between the two. Do not reintroduce inline implementations here.
  private def dartClassName(cn: String): String = DartNames.dartClassName(cn)
  private def dartSubtypeName(key: String, defn: Definition, parentDartName: Option[String] = None): String =
    DartNames.dartSubtypeName(key, defn, parentDartName)

  /** Rename fields that would be private in Dart (underscore prefix). */
  private def dartFieldName(name: String): String = if (name.startsWith("_")) name.drop(1) else name

  /** Effective list of (name, Definition) to emit as Dart types. Auto-includes the wireType
    * so the user doesn't have to also add it to `defTypes`. */
  private lazy val effectiveDefTypes: List[(String, Definition)] = {
    val (wireName, wireDefn) = config.wireType
    if (config.defTypes.exists(_._1 == wireName)) config.defTypes
    else config.defTypes :+ (wireName -> wireDefn)
  }

  def generate(): List[SourceFile] = {
    val base = List(generateClient(), generateHandler(), generateSender(), generateRestClient())
    if (effectiveDefTypes.nonEmpty || config.enums.nonEmpty || config.typedRestTools.nonEmpty)
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
    val (typeName, _) = config.wireType
    val typesImport = s"import '${snakeCase(sn)}_types.dart';"
    val getterName = s"on$typeName"
    val typedGetter =
      s"""
         |  /// Typed durable events from the server, deserialized via $typeName.fromJson.
         |  Stream<(int, $typeName)> get $getterName =>
         |      _eventController.stream.map((e) => (e.${"$"}1, $typeName.fromJson(e.${"$"}2)));""".stripMargin
    val paramName = s"${typeName.head.toLower}${typeName.tail}"
    val push =
      if (config.durableSubtypes.isEmpty) {
        // Legacy behavior — every push is framed as a durable Event.
        s"""  /// Push a typed $typeName to the server. Returns the assigned sequence number.
           |  int push($typeName $paramName) {
           |    _outboundSeq++;
           |    final seq = _outboundSeq;
           |    _sendRaw({'type': 'event', 'seq': seq, 'data': $paramName.toJson()});
           |    return seq;
           |  }""".stripMargin
      } else {
        // Subtype-aware framing. Wire-type subtypes whose discriminator is in
        // `durableSubtypes` go through the durable channel (sequenced,
        // replayable). Everything else (Notices, Deltas) goes ephemeral —
        // server-side `protocol.onEvent` doesn't try to decode them as Events,
        // and they don't burn a sequence number.
        val durableLiteral = config.durableSubtypes.toList.sorted.map(s => s"'$s'").mkString(", ")
        s"""  static const Set<String> _durableSubtypes = {$durableLiteral};
           |
           |  /// Push a typed $typeName to the server. Subtypes whose discriminator
           |  /// is in `_durableSubtypes` are framed via the durable channel and
           |  /// receive a sequence number; everything else is framed via the
           |  /// ephemeral channel (returns `0`, no sequence assigned).
           |  int push($typeName $paramName) {
           |    final json = $paramName.toJson();
           |    final discriminator = json['type'] as String?;
           |    if (discriminator != null && _durableSubtypes.contains(discriminator)) {
           |      _outboundSeq++;
           |      final seq = _outboundSeq;
           |      _sendRaw({'type': 'event', 'seq': seq, 'data': json});
           |      return seq;
           |    } else {
           |      _sendRaw(json);
           |      return 0;
           |    }
           |  }""".stripMargin
      }
    val source = ClientTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%INFO_CLASS%%", generateInfoClass())
      .replace("%%INFO_TO_JSON%%", infoToJson())
      .replace("%%WIRE_IMPORT%%", typesImport)
      .replace("%%TYPED_STREAM_GETTER%%", typedGetter)
      .replace("%%PUSH_METHOD%%", push)

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

    val (wireTypeName, _) = config.wireType
    val wireParam = s"${wireTypeName.head.toLower}${wireTypeName.tail}"

    HandlerTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%SERVICE_SNAKE%%", snakeCase(sn))
      .replace("%%EVENT_STUBS%%", eventStubs)
      .replace("%%EPHEMERAL_STUBS%%", ephemeralStubs)
      .replace("%%EVENT_CASES%%", eventCases)
      .replace("%%EPHEMERAL_CASES%%", ephemeralCases)
      .replace("%%WIRE_TYPE%%", wireTypeName)
      .replace("%%WIRE_PARAM%%", wireParam)
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
    val (wireTypeName, _) = config.wireType

    StoredHandlerTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%TYPES_IMPORT%%", typesImport)
      .replace("%%TRANSIENT_STUBS%%", transientStubs)
      .replace("%%TRANSIENT_CASES%%", transientCases)
      .replace("%%WIRE_TYPE%%", wireTypeName)
  }

  // ---------------------------------------------------------------------------
  // Sender
  // ---------------------------------------------------------------------------

  private def generateSender(): SourceFile = {
    // Typed methods from clientEventDefs — construct the wire-typed class and pass it to push().
    // No `.toJson()` here: the sender's `push` (and the underlying client `push`) already takes
    // the wire type and serializes internally.
    val methods = config.clientEventDefs.flatMap { case (name, defn) =>
      defn.defType match {
        case o: DefType.Obj if o.map.nonEmpty =>
          val dartClass = name
          val stripped = name.replaceAll("^Client", "")
          val methodName = s"${stripped.head.toLower}${stripped.tail}"
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
               |    push($dartClass($args));
               |  }""".stripMargin)
        case _: DefType.Obj =>
          // Empty class (no fields)
          val dartClass = name
          val stripped = name.replaceAll("^Client", "")
          val methodName = s"${stripped.head.toLower}${stripped.tail}"
          Some(
            s"""  void $methodName() {
               |    push($dartClass());
               |  }""".stripMargin)
        case _ => None
      }
    }

    val allMethods = methods.mkString("\n\n")
    // Always import the wire-type's class file (sender's `push` takes wireType).
    val typeImports = s"import '${snakeCase(sn)}_types.dart';\n"
    val (wireTypeName, _) = config.wireType
    val wireParam = s"${wireTypeName.head.toLower}${wireTypeName.tail}"

    val source = SenderTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%SERVICE_SNAKE%%", snakeCase(sn))
      .replace("%%TYPE_IMPORTS%%", typeImports)
      .replace("%%METHODS%%", allMethods)
      .replace("%%WIRE_TYPE%%", wireTypeName)
      .replace("%%WIRE_PARAM%%", wireParam)

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
        case (_: DefType.Obj, Some(cn)) => dartClassName(cn)
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
    effectiveDefTypes.foreach { case (name, defn) => collectTypes(name, defn, toEmit) }
    config.typedRestTools.foreach { tool =>
      tool.inputDef.defType match {
        case o: DefType.Obj => o.map.foreach { case (_, fDefn) => collectNestedType(fDefn, toEmit) }
        case _ =>
      }
      (tool.outputDef.defType, tool.outputDef.className) match {
        case (_: DefType.Obj, Some(cn)) =>
          collectTypes(dartClassName(cn), tool.outputDef, toEmit)
        case _ =>
      }
    }

    // Build child→parent map from poly types (exclude simple enums — their values are enum constants, not class subtypes)
    val childToParent = mutable.Map.empty[String, String]
    // Build subtype Dart-class-name → polymorphic key map. The poly key is
    // Fabric's wire discriminator (`Product.productPrefix` for case classes,
    // the enum case name for Scala 3 enums). The parent's `fromJson`
    // dispatcher already keys on this; the subtype's `toJson` MUST emit the
    // same string or round-trips fail with "Unknown type discriminator". For
    // parameterless / anonymous enum cases (className `X$$anon$N`) the only
    // reliable source of the wire name is the poly key — `defn.className`
    // is junk for those.
    val subtypeWireKey = mutable.Map.empty[String, String]
    toEmit.foreach { case (name, defn) =>
      defn.defType match {
        case p: DefType.Poly if !isSimpleEnum(p) =>
          p.values.foreach { case (key, subDefn) =>
            val sn = dartSubtypeName(key, subDefn, Some(name))
            if (!childToParent.contains(sn)) childToParent(sn) = name
            if (!subtypeWireKey.contains(sn)) subtypeWireKey(sn) = key
          }
        case _ =>
      }
    }
    val polyTypes = toEmit.collect { case (name, defn) if defn.defType.isInstanceOf[DefType.Poly] =>
      val p = defn.defType.asInstanceOf[DefType.Poly]
      name -> p.values.map { case (key, subDefn) => dartSubtypeName(key, subDefn, Some(name)) }.toSet
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
      val dn = name
      val subDir = defn.className.map(packageSubDir).getOrElse("")
      typeLocations(dn) = (dn, subDir)
      // Also map poly subtypes
      defn.defType match {
        case p: DefType.Poly =>
          p.values.foreach { case (key, subDefn) =>
            val subName = dartSubtypeName(key, subDefn, Some(dn))
            val subSubDir = subDefn.className.map(packageSubDir).getOrElse(subDir)
            typeLocations(subName) = (subName, subSubDir)
          }
        case _ =>
      }
    }

    /** Resolve an import path from one type's subDir to another type's file.
      * Delegates relative-path computation to `DartNames.relativeImport` so the
      * cross-package import logic stays consistent with `OpenAPIDartGenerator`. */
    def importPath(fromSubDir: String, targetTypeName: String): String = {
      val (_, targetSubDir) = typeLocations.getOrElse(targetTypeName, (targetTypeName, ""))
      val targetFile = s"${snakeCase(targetTypeName)}.dart"
      DartNames.relativeImport(fromSubDir, targetSubDir, targetFile)
    }

    // Generate each type
    toEmit.foreach { case (name, defn) =>
      if (!dartReserved.contains(name)) {
        val dn = name
        val fileName = s"${snakeCase(dn)}.dart"
        val (_, subDir) = typeLocations.getOrElse(dn, (dn, ""))
        val filePath = if (subDir.nonEmpty) s"$modelPath/$subDir" else modelPath
        val parent = childToParent.get(name)

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
            // Wire discriminator: prefer the polymorphic key (the parent's
            // `p.values` key — guaranteed correct for `fromJson` dispatch).
            // Fall back to the className-derived leaf for top-level classes
            // that aren't poly subtypes. Anonymous Scala 3 enum cases
            // (`MessageVisibility$$anon$N`) would otherwise resolve to junk
            // ("MessageVisibility" or worse), breaking server-side decode.
            val wireName = subtypeWireKey.getOrElse(name,
              defn.className.map(cn => parseClassName(cn)._1).getOrElse(name)
            )
            files += SourceFile("Dart", dn, fileName, filePath,
              wrapClass(dn, fileName, generateDartClassBody(dn, wireName, o, toEmit, parent), allImports, hasFields = o.map.nonEmpty))
            exportNames += (if (subDir.nonEmpty) s"$subDir/$fileName" else fileName)
          case p: DefType.Poly =>
            val subImports = p.values.map { case (key, subDefn) =>
              val subName = dartSubtypeName(key, subDefn, Some(dn))
              s"import '${importPath(subDir, subName)}';"
            }.toList
            val parentImport = parent.map(p => s"import '${importPath(subDir, p)}';").toList
            val commonFieldImports = detectCommonFieldImports(p).map(t => s"import '${importPath(subDir, t)}';")
            val allImports = (subImports ++ parentImport ++ commonFieldImports).distinct.sorted
            files += SourceFile("Dart", dn, fileName, filePath,
              wrapPoly(dn, generateDartPoly(dn, p, toEmit, parent), allImports))
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
      case DefType.Poly(v, _) => v.values.foreach(scanForWrappers)
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
            val name = dartClassName(cn)
            if (name != selfName) names += name
          case _: DefType.Obj =>
            val name = dartClassName(cn)
            if (name != selfName) names += name
          case _: DefType.Poly if isSimpleEnum(d.defType.asInstanceOf[DefType.Poly]) =>
            val name = dartClassName(cn)
            if (name != selfName && !dartReserved.contains(name)) names += name
          case _: DefType.Poly =>
            val name = dartClassName(cn)
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

  /** Generate the class body (without file wrapper) for a concrete class.
    *
    * `dartName` is the (possibly renamed) Dart class name — used for the
    * `class X` declaration and constructor name. `wireName` is the
    * original Scala class name used by Fabric's poly discriminator —
    * embedded in `toJson()['type']` so the server-side poly RW
    * recognizes the value when it's read back. The two diverge for
    * Flutter-reserved names like `Text`/`TextContent`,
    * `Image`/`ImageContent` (see [[dartRename]]).
    */
  private def generateDartClassBody(
    dartName: String,
    wireName: String,
    o: DefType.Obj,
    knownTypes: mutable.LinkedHashMap[String, Definition],
    parentName: Option[String] = None
  ): String = generateDartClass(dartName, wireName, o, knownTypes, parentName)

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
          p.values.foreach { case (subName, subDefn) =>
            collectTypes(dartSubtypeName(subName, subDefn, Some(name)), subDefn, out)
          }
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
      // For nested case-class fields, register under the class-chain
      // qualified name (`dartClassName`) so the standalone file we emit
      // lands under the same key `defTypeToDartType` produces for the
      // field-reference side. Using `cn.split('.').last` (the bare leaf)
      // registers nested companion-object case classes like `Outer.Inner`
      // under `"Inner"` while the parent references them as `"OuterInner"`
      // — the mismatch produced files at the wrong path, an unresolvable
      // import, and an `InvalidType` cascade in the generated `.g.dart`.
      case _: DefType.Obj if cn.isDefined => collectTypes(DartNames.dartClassName(cn.get), defn, out)
      // Polys are referenced via leaf-style names by historical convention
      // (consumers' tests assert `abstract class Animal`, not the qualified
      // chain) — keep `cn.split('.').last` for the Poly path. Subtype names
      // inside the poly come from `dartSubtypeName` which already applies
      // qualification, so cross-poly collisions are resolved at the subtype
      // layer, not the parent layer.
      case _: DefType.Poly if cn.isDefined => collectTypes(cn.get.split('.').last, defn, out)
      case DefType.Arr(inner) => collectNestedType(inner, out)
      case DefType.Opt(inner) => collectNestedType(inner, out)
      case _ => ()
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
  private def generateDartEnumFromPoly(name: String, p: DefType.Poly): String =
    renderDartEnum(name, p.values.keys.toList)

  private def generateSimpleDartEnum(name: String, values: List[String]): String =
    renderDartEnum(name, values)

  /** Render a Dart enum with both case-insensitive `fromString` decoding
    * AND a `wireName` getter that returns the *original* Scala case
    * name. Dart's `.name` lowercases the first character (we have to —
    * Dart enum cases must start with a lowercase letter to avoid lint
    * errors), so using `.name` directly on the wire would emit
    * `"complete"` while the Scala server's case-sensitive `RW[T]`
    * expects `"Complete"`. `wireName` preserves the original. The
    * generated `toJson` paths (see `defTypeToJsonExpr`) use it. */
  private def renderDartEnum(name: String, originals: List[String]): String = {
    val cases = originals.map(dartIdentifier).mkString(",\n  ")
    val wireMap = originals.map { orig =>
      s"    $name.${dartIdentifier(orig)}: '$orig'"
    }.mkString(",\n")
    s"""enum $name {
       |  $cases;
       |
       |  /** Maps each Dart enum case back to its original Scala case
       |    * name (capitalized) so `toJson` round-trips against
       |    * fabric's case-sensitive poly RW. */
       |  static const Map<$name, String> _wireNames = {
       |$wireMap
       |  };
       |
       |  /** Original Scala case name — used by `toJson`. */
       |  String get wireName => _wireNames[this]!;
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
    dartName: String,
    wireName: String,
    o: DefType.Obj,
    knownTypes: mutable.LinkedHashMap[String, Definition],
    parentName: Option[String] = None
  ): String = {
    val extendsClause = parentName.map(p => s" extends $p").getOrElse("")
    // Discriminator on the wire is Fabric's poly key (original Scala class
    // name), not the Dart-renamed class name. Otherwise the server-side
    // poly write rejects the payload with "Unknown type discriminator" for
    // every renamed class (Text/TextContent, Image/ImageContent, …).
    val typeValue = wireName
    if (o.map.isEmpty) {
      // Empty class (e.g., case object with no fields)
      s"""class $dartName$extendsClause {
         |  $dartName();
         |  $dartName.fromJson(Map<String, dynamic> json);
         |
         |  Map<String, dynamic> toJson() => {'type': '$typeValue'};
         |}""".stripMargin
    } else {
      val rendered = o.map.toList.map { case (fname, fDefn) =>
        fname -> renderClassField(fname, fDefn, dartFieldName(fname))
      }
      val fields = rendered.map { case (_, r) => r.declaration }.mkString("\n")
      val ctorParams = rendered.map { case (_, r) => r.ctorParam }.mkString(", ")
      val fromJsonInits = rendered.map { case (_, r) => r.fromJsonInit }.mkString(",\n        ")
      val toJsonEntries = rendered.map { case (_, r) => r.toJsonEntry }.mkString(", ")

      s"""class $dartName$extendsClause {
         |$fields
         |
         |  $dartName({$ctorParams});
         |
         |  $dartName.fromJson(Map<String, dynamic> json)
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
    parentName: Option[String] = None
  ): String = {
    val extendsClause = parentName.map(p => s" extends $p").getOrElse("")
    val cases = p.values.map { case (key, defn) =>
      val subName = dartSubtypeName(key, defn, Some(name))
      // Discriminator value matches Fabric's wire format: simple class name (Product.productPrefix).
      // The Poly key from the macro IS the simple name.
      s"""    if (type == '$key') return $subName.fromJson(json);"""
    }.mkString("\n")

    // Common-field abstract getters come straight from the polytype's
    // `commonFields` map. Fabric's `PolyType.register` computes that as
    // the type-compatible intersection of subtype fields, so the codegen
    // doesn't re-derive it here. Single-subtype polys correctly emit
    // every field as a getter (intersection of one set is itself);
    // multi-subtype polys emit only the names every subtype actually
    // carries with a matching type. The discriminator `type` field is
    // never in commonFields by construction (it's a wire artifact, not
    // a subtype field). Sort for deterministic output.
    //
    // Bug #48 — `defTypeToDartType` already encodes `DefType.Opt` as
    // a trailing `?`, so the abstract getter's type comes from the
    // unwrapped definition. Appending another `?` here produced
    // `T??` (invalid Dart) for `Option[T]` commonFields.
    val commonGetters = p.commonFields.toList.sortBy(_._1).map { case (fieldName, fieldDefn) =>
      s"  ${defTypeToDartType(fieldDefn)} get ${dartFieldName(fieldName)};"
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

  /** Per-field Dart-rendering bundle: declaration line, constructor param,
    * fromJson init, and toJson entry. Computed once per field via
    * [[renderClassField]] so the four sites stay consistent. */
  private case class ClassFieldRender(declaration: String,
                                       ctorParam: String,
                                       fromJsonInit: String,
                                       toJsonEntry: String)

  /** Distinguish required vs defaulted vs true-Optional fields so a defaulted
    * primitive (`field: Boolean = false`) renders as `final bool field;` with
    * `this.field = false` in the ctor — the default value goes into the Dart
    * class declaration instead of the field becoming nullable. Falls back to
    * the nullable shape when the default isn't a primitive literal we can
    * render (collections of objects, nested case classes, etc.). */
  private def renderClassField(fname: String, fDefn: Definition, dartName: String): ClassFieldRender = {
    val docComment = fDefn.description.map(d => s"  /// $d\n").getOrElse("")
    val deprecatedAnnotation = if (fDefn.deprecated) s"  @Deprecated('Deprecated field')\n" else ""
    // Fabric ≤1.27 wrapped every defaulted field in `DefType.Opt(inner)` so the
    // JSON decoder tolerates a missing key. Fabric 1.28+ stops wrapping defaulted
    // non-Option fields in `Opt` — the field's `defType` is now the bare inner
    // type (e.g. `DefType.Bool`) with `defaultValue` still set on the outer
    // Definition. We must handle both shapes:
    //   - 1.27 defaulted Boolean: `Opt(Bool)` + `defaultValue = Some(Bool(false))`
    //   - 1.28 defaulted Boolean: `Bool`        + `defaultValue = Some(Bool(false))`
    // Either way we unwrap to the inner type and inline the literal as a Dart
    // default. `Option[T] = None` (still `Opt(inner)` + `defaultValue = JSON null`
    // under 1.28) takes the nullable Opt path — the `json != fabric.Null` guard
    // prevents the broken `final T field; this.field = null` rendering.
    val defaultedInner: Option[(Definition, String)] = fDefn.defaultValue match {
      case Some(json) if json != fabric.Null =>
        val inner = fDefn.defType match {
          case DefType.Opt(i) => i
          case _              => fDefn
        }
        dartLiteralOption(json, inner).map(lit => (inner, lit))
      case _ => None
    }

    defaultedInner match {
      case Some((inner, literal)) =>
        val typeDart = defTypeToDartType(inner)
        ClassFieldRender(
          declaration  = s"$docComment$deprecatedAnnotation  final $typeDart $dartName;",
          ctorParam    = s"this.$dartName = $literal",
          fromJsonInit = s"$dartName = ${defTypeFromJsonExpr(s"json['$fname']", inner)}",
          toJsonEntry  = s"'$fname': ${defTypeToJsonExpr(dartName, inner)}"
        )
      case None =>
        val typeDart = defTypeToDartType(fDefn)
        val ctor = if (typeDart.endsWith("?")) s"this.$dartName" else s"required this.$dartName"
        ClassFieldRender(
          declaration  = s"$docComment$deprecatedAnnotation  final $typeDart $dartName;",
          ctorParam    = ctor,
          fromJsonInit = s"$dartName = ${defTypeFromJson(fname, fDefn)}",
          toJsonEntry  = s"'$fname': ${defTypeToJsonExpr(dartName, fDefn)}"
        )
    }
  }

  /** Render a fabric Json value as a Dart literal expression. Returns `None`
    * for shapes that can't be safely round-tripped as a Dart literal — the
    * caller falls back to the nullable representation in that case. The
    * `_inner` parameter exists for future shape-aware widening (e.g. an
    * empty-list default on a typed List field) but is currently unused. */
  private def dartLiteralOption(json: fabric.Json, _inner: Definition): Option[String] = json match {
    case fabric.Bool(v, _)    => Some(v.toString)
    case fabric.NumInt(v, _)  => Some(v.toString)
    case fabric.NumDec(v, _)  => Some(v.toString)
    case fabric.Str(v, _)     => Some("'" + v.replace("\\", "\\\\").replace("'", "\\'").replace("\n", "\\n").replace("\r", "\\r") + "'")
    case fabric.Null          => Some("null")
    case fabric.Arr(v, _) if v.isEmpty => Some("const []")
    case _ => None
  }

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

  /** Collect Dart class names appearing as common fields of a Poly, excluding
    * primitives. Caller resolves paths to produce import statements. Reads
    * `p.commonFields` directly — fabric pre-computed the type-compatible
    * intersection at registration time.
    *
    * Bug #48 — strip a trailing `?` (from `DefType.Opt` wrappers on
    * `Option[T]` fields) so the filter sees the underlying type.
    *
    * Bug #47 — recurse into `List<X>` and `Map<K, V>` to pull their
    * inner type names. Filtering on the `List<` / `Map<` prefix
    * dropped the whole entry, which lost the import for any typed
    * inner — generated abstract Dart classes referenced types that
    * were never imported. */
  private def detectCommonFieldImports(p: DefType.Poly): List[String] = {
    val primitives = Set("String", "int", "double", "bool", "num", "dynamic", "Object", "void", "List", "Map")

    def extractTypes(dartType: String): Set[String] = {
      val cleaned = dartType.stripSuffix("?")
      if (cleaned.startsWith("List<") && cleaned.endsWith(">"))
        extractTypes(cleaned.substring(5, cleaned.length - 1))
      else if (cleaned.startsWith("Map<") && cleaned.endsWith(">")) {
        val inner = cleaned.substring(4, cleaned.length - 1)
        var depth = 0
        var split = -1
        var i     = 0
        while (i < inner.length && split < 0) {
          inner.charAt(i) match {
            case '<' => depth += 1
            case '>' => depth -= 1
            case ',' if depth == 0 => split = i
            case _   =>
          }
          i += 1
        }
        if (split < 0) extractTypes(inner)
        else extractTypes(inner.substring(0, split).trim) ++ extractTypes(inner.substring(split + 1).trim)
      } else if (primitives.contains(cleaned)) Set.empty
      else Set(cleaned)
    }

    (p.commonFields - "type").values
      .flatMap(d => extractTypes(defTypeToDartType(d)))
      .toSet
      .toList
  }

  /** Track typed wrappers discovered during generation. Key = base className (no type params), Value = (dartName, primitiveDartType). */
  private val discoveredWrappers = scala.collection.mutable.LinkedHashMap.empty[String, (String, String)]

  /** Parse a className like "lightdb.id.Id[User]" into ("Id", Some(List("User"))).
    * Returns (baseName, typeArgs) where typeArgs is None if not parameterized.
    *
    * Leaf-name extraction delegates to `DartNames.wireDiscriminator` so the rule for
    * "what is the simple name" stays in lockstep with `OpenAPIDartGenerator`. The type
    * argument split is local because Dart-side generic args are unique to this generator. */
  private def parseClassName(cn: String): (String, Option[List[String]]) = {
    val short = DartNames.wireDiscriminator(cn)
    val bracketIdx = cn.indexOf('[')
    if (bracketIdx == -1) (short, None)
    else {
      val argsStr = cn.substring(bracketIdx + 1, cn.lastIndexOf(']'))
      val args = argsStr.split(',').map(_.trim).toList
      (short, Some(args))
    }
  }

  /** Extract the base className (without type parameters) from a full className string.
    * Delegates to `DartNames.stripTypeArgs` so the type-arg stripping rule is shared. */
  private def baseClassName(cn: String): String = DartNames.stripTypeArgs(cn)

  /** Extract the full package path from a className as a slash-separated string.
    * Delegates to `DartNames.packagePath` so the package/class-chain split rule is shared. */
  private def packageSubDir(cn: String): String = DartNames.packagePath(cn)

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
            // Use class-chain concatenation so nested types like
            // `ResponseContent.Text` render as `ResponseContentText`,
            // matching OpenAPIDartGenerator's naming and avoiding
            // cross-poly collisions.
            dartClassName(cn)
          case _: DefType.Poly =>
            dartClassName(cn)
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
              val sub = dartClassName(cn)
              if (dartReserved.contains(sub)) s"$access as String?"
              else s"$sub.fromString($access as String?)"
            case None => s"$access as String?"
          }
        case _: DefType.Obj =>
          inner.className match {
            case Some(cn) => s"$access != null ? ${dartClassName(cn)}.fromJson($access as Map<String, dynamic>) : null"
            case None => s"$access as $innerDart?"
          }
        case _: DefType.Poly =>
          inner.className match {
            case Some(cn) => s"$access != null ? ${dartClassName(cn)}.fromJson($access as Map<String, dynamic>) : null"
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
              val sub = dartClassName(cn)
              s"($access as List?)?.map((e) => $sub.fromJson(e as Map<String, dynamic>)).toList() ?? []"
            case None =>
              s"($access as List?)?.cast<$innerDart>() ?? []"
          }
        // BUGS.md #18 — enum-shaped poly inside a collection must use the
        // generated `fromString`/`.wireName` API, not `fromJson`/`toJson`.
        // Mirrors the scalar-field branch above; without this the generator
        // emits `Foo.fromJson(e as Map<String, dynamic>)` for every element
        // and the resulting Dart fails to compile (Foo is an enum).
        case p: DefType.Poly if isSimpleEnum(p) =>
          inner.className match {
            case Some(cn) =>
              val sub = dartClassName(cn)
              if (dartReserved.contains(sub)) s"($access as List?)?.cast<String>() ?? []"
              else s"($access as List?)?.map((e) => $sub.fromString(e as String?)).whereType<$sub>().toList() ?? []"
            case None =>
              s"($access as List?)?.cast<$innerDart>() ?? []"
          }
        case _: DefType.Poly =>
          inner.className match {
            case Some(cn) =>
              val sub = dartClassName(cn)
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
          val sub = dartClassName(cn)
          if (dartReserved.contains(sub)) s"$access as String? ?? ''"
          else s"$sub.fromString($access as String?) ?? $sub.values.first"
        case None => s"$access as String? ?? ''"
      }
    case _: DefType.Obj =>
      d.className match {
        case Some(cn) =>
          val sub = dartClassName(cn)
          s"$sub.fromJson($access as Map<String, dynamic>)"
        case None => s"$access as dynamic"
      }
    case _: DefType.Poly =>
      d.className match {
        case Some(cn) =>
          val sub = dartClassName(cn)
          s"$sub.fromJson($access as Map<String, dynamic>)"
        case None => s"$access as dynamic"
      }
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
          // BUGS.md #18 — enum-shaped poly inside a collection serializes
          // via `wireName`, not `toJson`. Same dispatch the scalar-field
          // branch already uses.
          case p: DefType.Poly if isSimpleEnum(p) && inner.className.isDefined =>
            val sub = dartClassName(inner.className.get)
            if (dartReserved.contains(sub)) access else s"$access.map((e) => e.wireName).toList()"
          case _: DefType.Poly if inner.className.isDefined => s"$access.map((e) => e.toJson()).toList()"
          case _ => access
        }
      case p: DefType.Poly if isSimpleEnum(p) =>
        d.className match {
          case Some(cn) =>
            val sub = dartClassName(cn)
            // `wireName` returns the original (case-preserved) Scala
            // case name so the server-side fabric RW round-trips. Using
            // Dart's built-in `.name` would emit lowercased values
            // (Dart cases must start lowercase) and mismatch fabric's
            // case-sensitive poly discriminator.
            if (dartReserved.contains(sub)) access else s"$access.wireName"
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

  private def snakeCase(name: String): String = DartNames.snakeCaseFile(name)

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
