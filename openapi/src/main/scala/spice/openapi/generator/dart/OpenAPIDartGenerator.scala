package spice.openapi.generator.dart

import cats.effect.unsafe.implicits.global
import fabric.{Json, Str}
import spice.http.HttpMethod
import spice.net.ContentType
import spice.openapi.{OpenAPI, OpenAPIContent, OpenAPISchema}
import spice.openapi.generator.{OpenAPIGenerator, OpenAPIGeneratorConfig, SourceFile}
import spice.streamer._

import scala.collection.mutable

object OpenAPIDartGenerator extends OpenAPIGenerator {
  private lazy val ModelTemplate: String = loadString("generator/dart/model.template")
  private lazy val ModelWithParamsTemplate: String = loadString("generator/dart/model_with_params.template")
  private lazy val ParentTemplate: String = loadString("generator/dart/parent.template")
  private lazy val EnumTemplate: String = loadString("generator/dart/enum.template")
  private lazy val ServiceTemplate: String = loadString("generator/dart/service.template")

  override protected def fileExtension: String = ".dart"

  override protected def generatedComment: String = "/// GENERATED CODE: Do not edit!"

  private implicit class StringExtras(s: String) {
    def ref2Type: String = s.substring(s.lastIndexOf('/') + 1)
    def type2File: String = {
      val pre = s.charAt(0).toLower
      val suffix = "\\p{Lu}".r.replaceAllIn(s.substring(1), m => {
        s"_${m.group(0).toLowerCase}"
      })
      s"$pre$suffix".replace(" ", "")
    }
    def dartType: String = s match {
      case "string" => "String"
      case "boolean" => "bool"
      case "integer" => "int"
      case "number" => "double"
      case "json" => "Map<String, dynamic>"
      case _ => throw new RuntimeException(s"Unsupported dart type: [$s]")
    }
    def param: String = s"this.$prop"
    def prop: String = renameMap.getOrElse(s, s)
  }

  private implicit class OpenAPIContentExtras(content: OpenAPIContent) {
    def ref: OpenAPISchema.Ref = content
      .content
      .head
      ._2
      .schema
      .asInstanceOf[OpenAPISchema.Ref]

    def refType: String = ref.ref.ref2Type

    def component(api: OpenAPI): OpenAPISchema.Component = {
      val componentName = ref.ref.substring(ref.ref.lastIndexOf('/') + 1)
      api.components.get.schemas(componentName).asInstanceOf[OpenAPISchema.Component]
    }
  }

  private lazy val renameMap = Map(
    "bool" -> "b",
    "_id" -> "id"
  )

  private def field(`type`: String, name: String, nullable: Boolean): String = {
    val rename = renameMap.get(name)
    val n = if (nullable) "?" else ""
    rename match {
      case Some(r) => s"@JsonKey(name: '$name') final ${`type`}$n $r;"
      case None => s"final ${`type`}$n $name;"
    }
  }

  override def generate(api: OpenAPI, config: OpenAPIGeneratorConfig): List[SourceFile] = {
    val service = generateService(api, config)
    val modelObjects = generatePaths(api, config)

    service :: modelObjects
  }

  def generatePaths(api: OpenAPI, config: OpenAPIGeneratorConfig): List[SourceFile] = {
    var parentFiles = Map.empty[String, SourceFile]
    def addParent(tn: String, `enum`: List[String] = Nil): Unit = {
      val typeName = tn.replace(" ", "")
      if (!parentFiles.contains(typeName)) {
        if (`enum`.nonEmpty) {
          val fileName = s"${typeName.type2File}.dart"
          val fields = `enum`.map { e =>
            s"""@JsonValue('$e')
               |  $e,""".stripMargin
          }.mkString("\n  ")
          val source = EnumTemplate
            .replace("%%CLASSNAME%%", typeName)
            .replace("%%FIELDS%%", fields)
          parentFiles += typeName -> SourceFile(
            language = "Dart",
            name = typeName,
            fileName = fileName,
            path = "lib/model",
            source = source
          )
        } else {
          val children = config.baseNames.find(_._1 == typeName).get._2.toList.sorted
          var imports = children.toSet
          val maps = children.map { child =>
            val components = api.components.get
            val component = components
              .schemas.getOrElse(child, throw new NullPointerException(s"Unable to find $child in ${components.schemas.keys.mkString(", ")}"))
              .asInstanceOf[OpenAPISchema.Component]
            component.properties.map {
              case (key, schema) =>
                val `type` = schema match {
                  case c: OpenAPISchema.Component if c.`type` == "array" => c.items.get match {
                    case c: OpenAPISchema.Component if c.nullable.contains(true) => s"List<${c.`type`.dartType}>?"
                    case c: OpenAPISchema.Component => s"List<${c.`type`.dartType}>"
                    case r: OpenAPISchema.Ref =>
                      val c = r.ref.substring(r.ref.lastIndexOf('/') + 1)
                      imports += c
                      val s = s"List<$c>"
                      if (r.nullable.contains(true)) {
                        s"$s?"
                      } else {
                        s
                      }
                    case o: OpenAPISchema.OneOf =>
                      val refs = o.schemas.map(_.asInstanceOf[OpenAPISchema.Ref].ref.ref2Type)
                      val parents: List[String] = refs.map(r => config.baseForTypeMap.getOrElse(r, throw new RuntimeException(s"No mapping defined for $r"))).distinct
                      val parentName = parents match {
                        case parent :: Nil => parent
                        case _ => throw new RuntimeException(s"Multiple parents found for ${refs.mkString(", ")}: ${parents.mkString(", ")}")
                      }
                      val c = parentName.type2File
                      imports = imports + c
                      addParent(parentName)
                      val s = s"List<$c>"
                      if (o.nullable.contains(true)) {
                        s"$s?"
                      } else {
                        s
                      }
                    case s => throw new UnsupportedOperationException(s"Unsupported array schema: $s")
                  }
                  case c: OpenAPISchema.Component if c.`enum`.nonEmpty =>
                    val parentName = config.baseForTypeMap(c.`enum`.head.asString)
                    imports = imports + parentName.type2File
                    if (c.nullable.contains(true)) {
                      s"$parentName?"
                    } else {
                      parentName
                    }
                  case c: OpenAPISchema.Component if c.nullable.contains(true) => s"${c.`type`.dartType}?"
                  case c: OpenAPISchema.Component => c.`type`.dartType
                  case r: OpenAPISchema.Ref =>
                    val c = r.ref.substring(r.ref.lastIndexOf('/') + 1)
                    imports += c
                    if (r.nullable.contains(true)) {
                      s"$c?"
                    } else {
                      c
                    }
                  case s => throw new RuntimeException(s"Unsupported schema: $s")
                }
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
          val importString = imports.map { c =>
            s"import '${c.type2File}.dart';"
          }.mkString("\n")
          val fromJson = children.map { c =>
            s"""if (t == '$c') {
               |      return ${c.replace(" ", "")}.fromJson(json);
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
            path = "lib/model",
            source = source
          )
        }
      }
    }
    val sourceFiles = api
      .components
      .toList
      .flatMap(_.schemas.toList)
      .map {
        case (tn, schema: OpenAPISchema.Component) =>
          val typeName = tn.replace(" ", "")
          var imports = Set.empty[String]
          val fileName = s"${typeName.type2File}.dart"
          val fields = schema.properties.toList.map {
            case (fieldName, OpenAPISchema.Ref(ref, nullable)) =>
              val modelType = ref.ref2Type
              imports = imports + modelType.type2File
              field(modelType, fieldName, nullable.getOrElse(false))
            case (fieldName, schema: OpenAPISchema.Component) if schema.`type` == "array" =>
              schema.items match {
                case Some(itemsSchema: OpenAPISchema.Component) => try {
                  val arrayType = itemsSchema.`type`.dartType
                  field(s"List<$arrayType>", fieldName, schema.nullable.getOrElse(false))
                } catch {
                  case t: Throwable => throw new RuntimeException(s"Failure on $fieldName - $itemsSchema", t)
                }
                case Some(itemsSchema: OpenAPISchema.Ref) =>
                  val arrayType = itemsSchema.ref.ref2Type
                  imports = imports + arrayType.type2File
                  field(s"List<$arrayType>", fieldName, schema.nullable.getOrElse(false))
                case Some(OpenAPISchema.OneOf(schemas, discriminator, nullable)) =>
                  val refs = schemas.map(_.asInstanceOf[OpenAPISchema.Ref].ref.ref2Type)
                  val parents: List[String] = refs.map(r => config.baseForTypeMap.getOrElse(r, throw new RuntimeException(s"No mapping defined for $r"))).distinct
                  val parentName = parents match {
                    case parent :: Nil => parent
                    case _ => throw new RuntimeException(s"Multiple parents found for ${refs.mkString(", ")}: ${parents.mkString(", ")}")
                  }
                  imports = imports + parentName.type2File
                  addParent(parentName)
                  field(s"List<$parentName>", fieldName, schema.nullable.getOrElse(false))
                case _ => throw new UnsupportedOperationException(s"Unsupported array schema for items: ${schema.items}")
              }
            case (fieldName, schema: OpenAPISchema.Component) if schema.`type` != "object" =>
              val fieldType = if (schema.`enum`.nonEmpty) {
                val `enum` = schema.`enum`.map {
                  case Str(s, _) => s
                  case json => throw new UnsupportedOperationException(s"Enum only supports Str: $json")
                }
                val baseType = config.baseForTypeMap.getOrElse(`enum`.head, throw new RuntimeException(s"No mapping defined for ${`enum`.head}"))
                imports = imports + baseType.type2File
                addParent(baseType, `enum`)
                baseType
              } else {
                schema.`type`.dartType
              }
              field(fieldType, fieldName, schema.nullable.getOrElse(false))
            case (fieldName, OpenAPISchema.OneOf(schemas, discriminator, nullable)) =>
              val refs = schemas.map(_.asInstanceOf[OpenAPISchema.Ref].ref.ref2Type)
              val parents: List[String] = refs.map(r => config.baseForTypeMap.getOrElse(r, throw new RuntimeException(s"No mapping defined for $r"))).distinct
              val parentName = parents match {
                case parent :: Nil => parent
                case _ => throw new RuntimeException(s"Multiple parents found for ${refs.mkString(", ")}: ${parents.mkString(", ")}")
              }
              imports = imports + parentName.type2File
              addParent(parentName)
              field(parentName, fieldName, nullable.getOrElse(false))
            case (fieldName, schema: OpenAPISchema.Component) if schema.additionalProperties.nonEmpty =>
              val (valueType, nullable) = schema.additionalProperties.get match {
                case valueSchema: OpenAPISchema.Component =>
                  val dartType = if (valueSchema.`type` == "array") {
                    valueSchema.items match {
                      case Some(itemSchema) => itemSchema match {
                        case c: OpenAPISchema.Component => c.`type`.dartType
                        case r: OpenAPISchema.Ref =>
                          val refType = r.ref.ref2Type
                          imports = imports + refType.type2File
                          val nullable = valueSchema.nullable.getOrElse(false)
                          scribe.info(s"4: $refType, $nullable")
                          s"List<$refType>${if (nullable) "?" else ""}"
                        case s => throw new UnsupportedOperationException(s"Unsupported item schema: $s")
                      }
                      case None =>
                        val nullable = valueSchema.nullable.getOrElse(false)
                        s"List<dynamic>${if (nullable) "?" else ""}"
                    }
                  } else {
                    valueSchema.`type`.dartType
                  }
                  dartType -> valueSchema.nullable
                case OpenAPISchema.Ref(ref, nullable) =>
                  val refType = ref.ref2Type
                  imports = imports + refType.type2File
                  refType -> nullable
                case OpenAPISchema.OneOf(schemas, discriminator, nullable) =>
                  val refs = schemas.map(_.asInstanceOf[OpenAPISchema.Ref].ref.ref2Type)
                  val parents: List[String] = refs.map(r => config.baseForTypeMap.getOrElse(r, throw new RuntimeException(s"No mapping defined for $r"))).distinct
                  val parentName = parents match {
                    case parent :: Nil => parent
                    case _ => throw new RuntimeException(s"Multiple parents found for ${refs.mkString(", ")}: ${parents.mkString(", ")}")
                  }
                  imports = imports + parentName.type2File
                  addParent(parentName)
                  (parentName, nullable)
                case valueSchema => throw new UnsupportedOperationException(s"$fieldName has unsupported value schema: $valueSchema")
              }
              field(s"Map<String, $valueType>", fieldName, nullable.getOrElse(false))
            case (fieldName, schema) => throw new UnsupportedOperationException(s"$fieldName has unsupported schema: $schema")
          }.mkString("\n  ") match {
            case "" => "// No fields defined"
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
          val parent = config.baseForTypeMap.get(tn)
          val extending = parent match {
            case Some(parentName) =>
              imports = imports + parentName.type2File
              s"extends $parentName with EquatableMixin "
            case None => "extends Equatable "
          }
          val importsTemplate = imports.toList.sorted.map(s => s"import '$s.dart';").mkString("\n") match {
            case "" => "// No imports necessary"
            case s => s
          }
          val toJson = parent match {
            case Some(_) =>
              s"""@override Map<String, dynamic> toJson() {
                 |    Map<String, dynamic> map = _$$${typeName}ToJson(this);
                 |    map['type'] = '$tn';
                 |    return map;
                 |  }""".stripMargin
            case None => s"Map<String, dynamic> toJson() => _$$${typeName}ToJson(this);"
          }
          val source = (if (params.isEmpty) ModelTemplate else ModelWithParamsTemplate)
            .replace("%%IMPORTS%%", importsTemplate)
            .replace("%%FILENAME%%", typeName.type2File)
            .replace("%%CLASSNAME%%", typeName)
            .replace("%%EXTENDS%%", extending)
            .replace("%%FIELDS%%", fields)
            .replace("%%PARAMS%%", params)
            .replace("%%PROPS%%", props)
            .replace("%%TOJSON%%", toJson)
          SourceFile(
            language = "Dart",
            name = typeName,
            fileName = fileName,
            path = "lib/model",
            source = source
          )
        case (typeName, schema) => throw new UnsupportedOperationException(s"$typeName has unsupported schema: $schema")
      }
    sourceFiles ::: parentFiles.values.toList
  }

  def generateService(api: OpenAPI, config: OpenAPIGeneratorConfig): SourceFile = {
    var imports = Set.empty[String]
    val methods: List[String] = api.paths.toList.sortBy(_._1).map {
      case (pathString, path) =>
        val name = "[^a-zA-Z0-9](\\S)".r.replaceAllIn(pathString.substring(1), m => {
          m.group(1).toUpperCase
        })
        val entry = path.methods(HttpMethod.Post)
        val requestContent = entry.requestBody.get.content
        val (requestContentType, apiContentType) = requestContent.content.head
        val successResponse = entry
          .responses("200")
          .content
        val (_, apiPath) = successResponse.content.head
        apiPath.schema match {
          case c: OpenAPISchema.Component => c.format match {
            case Some("binary") =>
              val requestType = requestContent.refType
              imports = imports + requestType.type2File
              s"""  /// ${entry.description}
                 |  static Future<void> $name($requestType request, String fileName) async {
                 |    await restDownload(fileName, "$pathString", request.toJson());
                 |  }""".stripMargin
            case format => throw new RuntimeException(s"Unsupported schema format: $format (schema: $c, path: $pathString)")
          }
          case _: OpenAPISchema.Ref =>
            val responseType = successResponse.refType
            val component = successResponse.component(api)
            val binary = component.format.contains("binary")
            imports = imports + responseType.type2File
            if (requestContentType == ContentType.`multipart/form-data`) {
              apiContentType.schema match {
                case c: OpenAPISchema.Component =>
                  val params = c.properties.toList.map {
                    case (key, schema) =>
                      val paramType = schema match {
                        case child: OpenAPISchema.Component if child.format.contains("binary") => "PlatformFile"
                        case child: OpenAPISchema.Component => s"${child.`type`.dartType}"
                        case ref: OpenAPISchema.Ref => ref.ref.ref2Type
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
                      imports = imports + ref.ref.ref2Type.type2File
                      s"${ws}request.fields['$key'] = json.encode($key.toJson());"
                    case (key, schema) => throw new UnsupportedOperationException(s"Unable to support $key: $schema")
                  }.mkString("\n")
                  s"""  /// ${entry.description}
                     |  static Future<$responseType> $name($params) async {
                     |    return await multiPart(
                     |      "$pathString",
                     |      (request) {
                     |$conversions
                     |      },
                     |      $responseType.fromJson
                     |    );
                     |  }""".stripMargin
                case _ => throw new UnsupportedOperationException(s"Unsupported schema: ${apiContentType.schema}")
              }
            } else if (binary) {
              val requestType = requestContent.refType
              imports = imports + requestType.type2File
              s"""  /// ${entry.description}
                 |  static Future<void> $name($requestType request) async {
                 |    return await restDownload(
                 |      downloadFileName,
                 |      "$pathString",
                 |      request.toJson()
                 |    );
                 |  }""".stripMargin
            } else {
              val requestType = requestContent.refType
              imports = imports + requestType.type2File
              s"""  /// ${entry.description}
                 |  static Future<$responseType> $name($requestType request) async {
                 |    return await restful(
                 |      "$pathString",
                 |      request.toJson(),
                 |      $responseType.fromJson
                 |    );
                 |  }""".stripMargin
            }
          case schema => throw new RuntimeException(s"Unsupported schema: $schema")
        }
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