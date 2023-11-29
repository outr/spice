package spice.openapi.generator.dart

import cats.effect.unsafe.implicits.global
import spice.http.HttpMethod
import spice.openapi.{OpenAPI, OpenAPIContent, OpenAPISchema}
import spice.openapi.generator.{OpenAPIGenerator, SourceFile}
import spice.streamer.Streamer

import scala.collection.mutable

object OpenAPIDartGenerator extends OpenAPIGenerator {
  private lazy val ModelTemplate: String = loadString("generator/dart/model.template")
  private lazy val ServiceTemplate: String = loadString("generator/dart/service.template")

  private implicit class StringExtras(s: String) {
    def ref2Type: String = s.substring(s.lastIndexOf('/') + 1)
    def type2File: String = {
      val pre = s.charAt(0).toLower
      val suffix = "\\p{Lu}".r.replaceAllIn(s.substring(1), m => {
        s"_${m.group(0).toLowerCase}"
      })
      s"$pre$suffix"
    }
    def dartType: String = s match {
      case "string" => "String"
    }
  }

  private implicit class OpenAPIContentExtras(content: OpenAPIContent) {
    def refType: String = content
      .content
      .head
      ._2
      .schema
      .asInstanceOf[OpenAPISchema.Ref]
      .ref
      .ref2Type
  }

  override def generate(api: OpenAPI): List[SourceFile] = {
    val service = generateService(api)
    val modelObjects = generatePaths(api)

    service :: modelObjects
  }

  def generatePaths(api: OpenAPI): List[SourceFile] = {
    api
      .components
      .toList
      .flatMap(_.schemas.toList)
      .map {
        case (typeName, schema: OpenAPISchema.Component) =>
          var imports = Set.empty[String]
          val fileName = s"${typeName.type2File}.dart"
          val fields = schema.properties.toList.map {
            case (fieldName, OpenAPISchema.Ref(ref)) =>
              val modelType = ref.ref2Type
              imports = imports + ref.ref2Type.type2File
              s"final $modelType $fieldName;"
            case (fieldName, schema: OpenAPISchema.Component) if schema.`type` == "array" =>
              val arrayType = schema.items.get.asInstanceOf[OpenAPISchema.Component].`type`.dartType
              s"final List<$arrayType> $fieldName;"
            case (fieldName, schema: OpenAPISchema.Component) if schema.`type` != "object" =>
              val fieldType = schema.`type`.dartType
              s"final $fieldType $fieldName;"
            case (fieldName, schema: OpenAPISchema.Component) if schema.additionalProperties.nonEmpty =>
              val valueType = schema.additionalProperties.get match {
                case valueSchema: OpenAPISchema.Component => valueSchema.`type`.dartType
                case valueSchema => throw new UnsupportedOperationException(s"$fieldName has unsupported value schema: $valueSchema")
              }
              s"final Map<String, $valueType> $fieldName;"
            case (fieldName, schema) => throw new UnsupportedOperationException(s"$fieldName has unsupported schema: $schema")
          }.mkString("\n  ")
          val params = schema.properties.toList.map {
            case (fieldName, _) => s"this.$fieldName"
          }.mkString(", ")
          val importsTemplate = imports.toList.sorted.map(s => s"import '$s.dart';").mkString("\n")
          val source = ModelTemplate
            .replace("%%IMPORTS%%", importsTemplate)
            .replace("%%FILENAME%%", typeName.type2File)
            .replace("%%CLASSNAME%%", typeName)
            .replace("%%FIELDS%%", fields)
            .replace("%%PARAMS%%", params)
          SourceFile(
            language = "Dart",
            name = typeName,
            fileName = fileName,
            path = "lib/model",
            source = source
          )
        case (typeName, schema) => throw new UnsupportedOperationException(s"$typeName has unsupported schema: $schema")
      }
  }

  def generateService(api: OpenAPI): SourceFile = {
    var imports = Set.empty[String]
    val methods = api.paths.toList.sortBy(_._1).map {
      case (pathString, path) =>
        val name = pathString.substring(1)
        val entry = path.methods(HttpMethod.Post)
        val requestType = entry
          .requestBody
          .get
          .content
          .refType
        val responseType = entry
          .responses("200")
          .content
          .refType
        imports = imports + requestType.type2File
        imports = imports + responseType.type2File
        s"""  /// ${entry.description}
           |  static Future<$responseType> $name($requestType request) async {
           |    return await restful(
           |      "$pathString",
           |      request.toJson(),
           |      $responseType.fromJson
           |    );
           |  }""".stripMargin
    }
    val importsTemplate = imports.toList.sorted.map(s => s"import 'model/$s.dart';").mkString("\n")
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
    ).unsafeRunSync().toString
  }
}