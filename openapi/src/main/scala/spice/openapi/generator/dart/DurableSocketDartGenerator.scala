package spice.openapi.generator.dart

import spice.api.server.{DurableEventDescriptor, DurableFieldDescriptor}
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
  infoFields: List[(String, String)] = Nil
)

/** Generates Dart code for a DurableSocket client, event handler, event sender,
  * and REST client from introspected type descriptors. */
case class DurableSocketDartGenerator(config: DurableSocketDartConfig) {
  private lazy val ClientTemplate: String = loadString("generator/dart/durable_client.template")
  private lazy val HandlerTemplate: String = loadString("generator/dart/durable_event_handler.template")
  private lazy val SenderTemplate: String = loadString("generator/dart/durable_event_sender.template")
  private lazy val RestTemplate: String = loadString("generator/dart/durable_rest_client.template")

  private val generatedComment = "/// GENERATED CODE: Do not edit!"
  private val sn = config.serviceName

  def generate(): List[SourceFile] = {
    List(generateClient(), generateHandler(), generateSender(), generateRestClient())
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

    val source = HandlerTemplate
      .replace("%%SERVICE_NAME%%", sn)
      .replace("%%SERVICE_SNAKE%%", snakeCase(sn))
      .replace("%%EVENT_STUBS%%", eventStubs)
      .replace("%%EPHEMERAL_STUBS%%", ephemeralStubs)
      .replace("%%EVENT_CASES%%", eventCases)
      .replace("%%EPHEMERAL_CASES%%", ephemeralCases)

    SourceFile("Dart", s"${sn}DurableHandler", s"${snakeCase(sn)}_durable_handler.dart", "lib/ws/durable", source)
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
  // Utilities
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

  private def camelCase(snakeStr: String): String = {
    val parts = snakeStr.split("_")
    parts.head + parts.tail.map(_.capitalize).mkString
  }

  private def snakeCase(name: String): String = {
    val first = name.charAt(0).toLower
    val rest = "\\p{Lu}".r.replaceAllIn(name.substring(1), m => s"_${m.group(0).toLowerCase}")
    s"$first$rest"
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
