package spice.openapi.generator.dart

import spice.api.server.{WsMethodDescriptor, WsParamDescriptor}
import spice.openapi.generator.SourceFile
import spice.streamer.*
import spice.streamer.given

import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.io.Source

case class WsDartGenerator(serviceName: String, methods: List[WsMethodDescriptor]) {
  private lazy val HandlerTemplate: String = loadString("generator/dart/ws_handler.template")
  private lazy val DispatcherTemplate: String = loadString("generator/dart/ws_dispatcher.template")
  private lazy val SenderTemplate: String = loadString("generator/dart/ws_sender.template")

  private val generatedComment = "/// GENERATED CODE: Do not edit!"

  def generate(): List[SourceFile] = {
    List(generateHandler(), generateDispatcher(), generateSender())
  }

  def write(sourceFiles: List[SourceFile], path: Path, deleteBeforeWrite: Boolean = true): Unit = {
    if (deleteBeforeWrite) {
      sourceFiles.map(_.path).distinct.foreach { filePath =>
        val directory = path.resolve(filePath).toFile
        directory.mkdirs()
        directory.listFiles().foreach { file =>
          if (isGenerated(file)) {
            if (!file.delete()) {
              file.deleteOnExit()
            }
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

  private def isGenerated(file: File): Boolean = {
    if (!file.isDirectory && file.getName.endsWith(".dart")) {
      val s = Source.fromFile(file)
      try {
        s.getLines().exists(_.contains(generatedComment))
      } finally {
        s.close()
      }
    } else false
  }

  def generateHandler(): SourceFile = {
    val methodLines = methods.map { m =>
      val params = m.params.map(p => s"${dartType(p)} ${p.name}").mkString(", ")
      s"  void ${m.name}($params) {}"
    }.mkString("\n")

    val source = HandlerTemplate
      .replace("%%SERVICE_NAME%%", serviceName)
      .replace("%%METHODS%%", methodLines)

    SourceFile(
      language = "Dart",
      name = s"${serviceName}Handler",
      fileName = s"${snakeCase(serviceName)}_handler.dart",
      path = "lib/ws",
      source = source
    )
  }

  def generateDispatcher(): SourceFile = {
    val cases = methods.map { m =>
      val args = m.params.map(p => dartArgExpr(p)).mkString(", ")
      s"      case '${m.name}':\n        handler.${m.name}($args);"
    }.mkString("\n")

    val handlerFile = s"${snakeCase(serviceName)}_handler.dart"
    val source = DispatcherTemplate
      .replace("%%SERVICE_NAME%%", serviceName)
      .replace("%%HANDLER_FILE%%", handlerFile)
      .replace("%%CASES%%", cases)

    SourceFile(
      language = "Dart",
      name = s"${serviceName}Dispatcher",
      fileName = s"${snakeCase(serviceName)}_dispatcher.dart",
      path = "lib/ws",
      source = source
    )
  }

  def generateSender(): SourceFile = {
    val methodLines = methods.map { m =>
      val params = m.params.map(p => s"${dartType(p)} ${p.name}").mkString(", ")
      val argEntries = m.params.map(p => s"'${p.name}': ${p.name}").mkString(", ")
      s"""  void ${m.name}($params) {
         |    send(jsonEncode({'method': '${m.name}', 'args': {$argEntries}}));
         |  }""".stripMargin
    }.mkString("\n\n")

    val source = SenderTemplate
      .replace("%%SERVICE_NAME%%", serviceName)
      .replace("%%METHODS%%", methodLines)

    SourceFile(
      language = "Dart",
      name = s"${serviceName}Sender",
      fileName = s"${snakeCase(serviceName)}_sender.dart",
      path = "lib/ws",
      source = source
    )
  }

  private def dartType(p: WsParamDescriptor): String = {
    val base = mapType(p.typeName)
    if (p.optional) s"$base?" else base
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
    case other => throw new RuntimeException(s"Unsupported type for Dart mapping: $other")
  }

  private def dartArgExpr(p: WsParamDescriptor): String = {
    val access = s"args['${p.name}']"
    if (p.optional) {
      p.typeName match {
        case s if s.startsWith("List[") =>
          val inner = mapType(s.substring(5, s.length - 1))
          s"$access != null ? ($access as List).cast<$inner>() : null"
        case _ =>
          s"$access as ${mapType(p.typeName)}?"
      }
    } else {
      p.typeName match {
        case s if s.startsWith("List[") =>
          val inner = mapType(s.substring(5, s.length - 1))
          s"($access as List).cast<$inner>()"
        case _ =>
          s"$access as ${mapType(p.typeName)}"
      }
    }
  }

  private def snakeCase(name: String): String = {
    val first = name.charAt(0).toLower
    val rest = "\\p{Lu}".r.replaceAllIn(name.substring(1), m => s"_${m.group(0).toLowerCase}")
    s"$first$rest"
  }

  private def loadString(name: String): String = {
    val stream = getClass.getClassLoader.getResourceAsStream(name)
    if (stream == null) throw new RuntimeException(s"Not found: $name")
    Streamer(stream, new mutable.StringBuilder).sync().toString
  }
}
