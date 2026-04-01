package spec

import fabric.*
import fabric.dsl.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.HttpMethod
import spice.net.*
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIContent, OpenAPIContentType, OpenAPIInfo, OpenAPIPath, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema, OpenAPIServer}
import spice.openapi.generator.{OpenAPIGeneratorConfig, SourceFile}
import spice.openapi.generator.dart.OpenAPIDartGenerator

/**
 * Pre-migration snapshot tests for the Dart generator.
 * These tests lock in the exact Dart output before upgrading to OpenAPI 3.2.0.
 * If any test here fails after the migration, it means the Dart generator's output changed.
 */
class DartGeneratorPreMigrationSpec extends AnyWordSpec with Matchers {

  "Dart Generator Pre-Migration Snapshots" should {

    "generate correct Dart enum from OpenAPI enum schema" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Status" -> OpenAPISchema.Component(
              `type` = "string",
              `enum` = List(str("active"), str("inactive"), str("pending"))
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val enumFile = result.find(_.fileName == "status.dart").get
      enumFile.source should include("enum Status")
      enumFile.source should include("@JsonValue('active')")
      enumFile.source should include("@JsonValue('inactive')")
      enumFile.source should include("@JsonValue('pending')")
      enumFile.source should include("active('active')")
      enumFile.source should include("inactive('inactive')")
      enumFile.source should include("pending('pending')")
      enumFile.source should include("final String label;")
      enumFile.source should include("const Status(this.label);")
    }

    "generate correct Dart model class with fields" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "User" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "age" -> OpenAPISchema.Component(`type` = "integer"),
                "score" -> OpenAPISchema.Component(`type` = "number"),
                "active" -> OpenAPISchema.Component(`type` = "boolean")
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val userFile = result.find(_.fileName == "user.dart").get
      userFile.source should include("class User extends Equatable")
      userFile.source should include("final String name;")
      userFile.source should include("final int age;")
      userFile.source should include("final double score;")
      userFile.source should include("final bool active;")
      userFile.source should include("static User fromJson(Map<String, dynamic> json) => _$UserFromJson(json);")
      userFile.source should include("Map<String, dynamic> toJson() => _$UserToJson(this);")
      userFile.source should include("@override")
      userFile.source should include("List<Object?> get props => [name, age, score, active];")
      userFile.source should include("{required this.name, required this.age, required this.score, required this.active}")
      userFile.source should include("User deepClone() => fromJson(toJson());")
    }

    "generate correct nullable field handling" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Profile" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "bio" -> OpenAPISchema.Component(`type` = "string", nullable = Some(true)),
                "age" -> OpenAPISchema.Component(`type` = "integer", nullable = Some(true))
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val profileFile = result.find(_.fileName == "profile.dart").get
      // Non-nullable field uses 'required'
      profileFile.source should include("required this.name")
      // Nullable fields should have ? suffix and no 'required'
      profileFile.source should include("final String? bio;")
      profileFile.source should include("final int? age;")
      profileFile.source should not include("required this.bio")
      profileFile.source should not include("required this.age")
      // Non-nullable params use 'required'
      profileFile.source should include("this.bio")
      profileFile.source should include("this.age")
    }

    "generate correct Dart for $ref fields" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Address" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "street" -> OpenAPISchema.Component(`type` = "string"),
                "city" -> OpenAPISchema.Component(`type` = "string")
              )
            ),
            "Person" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "address" -> OpenAPISchema.Ref("#/components/schemas/Address")
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val personFile = result.find(_.fileName == "person.dart").get
      personFile.source should include("import 'address.dart';")
      personFile.source should include("final Address address;")
      personFile.source should include("required this.address")
    }

    "generate correct Dart for nullable $ref fields" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Tag" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "label" -> OpenAPISchema.Component(`type` = "string")
              )
            ),
            "Item" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "tag" -> OpenAPISchema.Ref("#/components/schemas/Tag", nullable = Some(true))
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val itemFile = result.find(_.fileName == "item.dart").get
      itemFile.source should include("final Tag? tag;")
      itemFile.source should not include("required this.tag")
    }

    "generate correct Dart for array fields" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Team" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "members" -> OpenAPISchema.Component(
                  `type` = "array",
                  items = Some(OpenAPISchema.Component(`type` = "string"))
                ),
                "scores" -> OpenAPISchema.Component(
                  `type` = "array",
                  items = Some(OpenAPISchema.Component(`type` = "integer"))
                )
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val teamFile = result.find(_.fileName == "team.dart").get
      teamFile.source should include("final List<String> members;")
      teamFile.source should include("final List<int> scores;")
    }

    "generate correct Dart for array of $ref" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Widget" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "id" -> OpenAPISchema.Component(`type` = "string")
              )
            ),
            "Dashboard" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "widgets" -> OpenAPISchema.Component(
                  `type` = "array",
                  items = Some(OpenAPISchema.Ref("#/components/schemas/Widget"))
                )
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val dashFile = result.find(_.fileName == "dashboard.dart").get
      dashFile.source should include("import 'widget.dart';")
      dashFile.source should include("final List<Widget> widgets;")
    }

    "generate correct Dart for map (additionalProperties) fields" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Config" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "settings" -> OpenAPISchema.Component(
                  `type` = "object",
                  additionalProperties = Some(OpenAPISchema.Component(`type` = "string"))
                )
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val configFile = result.find(_.fileName == "config.dart").get
      configFile.source should include("final Map<String, String> settings;")
    }

    "generate correct Dart for map of $ref" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Entry" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "value" -> OpenAPISchema.Component(`type` = "integer")
              )
            ),
            "Registry" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "entries" -> OpenAPISchema.Component(
                  `type` = "object",
                  additionalProperties = Some(OpenAPISchema.Ref("#/components/schemas/Entry"))
                )
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val regFile = result.find(_.fileName == "registry.dart").get
      regFile.source should include("import 'entry.dart';")
      regFile.source should include("final Map<String, Entry> entries;")
    }

    "generate correct service for POST endpoint" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        paths = Map(
          "/reverse" -> OpenAPIPath(
            methods = Map(
              HttpMethod.Post -> OpenAPIPathEntry(
                summary = "Reverse text",
                description = "Reverses the given text",
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "OK",
                    content = Some(OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Ref("#/components/schemas/ReverseResponse")
                      )
                    ))
                  )
                ),
                requestBody = Some(OpenAPIRequestBody(
                  required = true,
                  content = OpenAPIContent(
                    ContentType.`application/json` -> OpenAPIContentType(
                      schema = OpenAPISchema.Ref("#/components/schemas/ReverseRequest")
                    )
                  )
                ))
              )
            )
          )
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "ReverseRequest" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map("text" -> OpenAPISchema.Component(`type` = "string"))
            ),
            "ReverseResponse" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map("reversed" -> OpenAPISchema.Component(`type` = "string"))
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val serviceFile = result.find(_.fileName == "service.dart").get
      serviceFile.source should include("import 'model/reverse_request.dart';")
      serviceFile.source should include("import 'model/reverse_response.dart';")
      serviceFile.source should include("static Future<ReverseResponse> reverse(ReverseRequest request) async")
      serviceFile.source should include("return await restful(")
      serviceFile.source should include("\"/reverse\"")
      serviceFile.source should include("request.toJson()")
      serviceFile.source should include("ReverseResponse.fromJson")
    }

    "generate correct service for GET endpoint" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        paths = Map(
          "/status" -> OpenAPIPath(
            methods = Map(
              HttpMethod.Get -> OpenAPIPathEntry(
                summary = "Get status",
                description = "Gets system status",
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "OK",
                    content = Some(OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Ref("#/components/schemas/StatusResponse")
                      )
                    ))
                  )
                )
              )
            )
          )
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "StatusResponse" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map("healthy" -> OpenAPISchema.Component(`type` = "boolean"))
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val serviceFile = result.find(_.fileName == "service.dart").get
      serviceFile.source should include("static Future<StatusResponse> status() async")
      serviceFile.source should include("return await restGet(")
      serviceFile.source should include("\"/status\"")
      serviceFile.source should include("StatusResponse.fromJson")
    }

    "generate correct service for void POST (null response type)" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        paths = Map(
          "/notify" -> OpenAPIPath(
            methods = Map(
              HttpMethod.Post -> OpenAPIPathEntry(
                summary = "Send notification",
                description = "Sends a notification",
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "OK",
                    content = Some(OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Component(`type` = "null")
                      )
                    ))
                  )
                ),
                requestBody = Some(OpenAPIRequestBody(
                  required = true,
                  content = OpenAPIContent(
                    ContentType.`application/json` -> OpenAPIContentType(
                      schema = OpenAPISchema.Ref("#/components/schemas/NotifyRequest")
                    )
                  )
                ))
              )
            )
          )
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "NotifyRequest" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map("message" -> OpenAPISchema.Component(`type` = "string"))
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val serviceFile = result.find(_.fileName == "service.dart").get
      serviceFile.source should include("static Future<void> notify(NotifyRequest request) async")
      serviceFile.source should include("await restPost(")
    }

    "generate correct parent class for polymorphic types" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Dog" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "breed" -> OpenAPISchema.Component(`type` = "string")
              )
            ),
            "Cat" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "color" -> OpenAPISchema.Component(`type` = "string")
              )
            ),
            "Animal" -> OpenAPISchema.OneOf(
              schemas = List(
                OpenAPISchema.Ref("#/components/schemas/Dog"),
                OpenAPISchema.Ref("#/components/schemas/Cat")
              ),
              discriminator = Some(OpenAPISchema.Discriminator(
                propertyName = "type",
                mapping = Map("Dog" -> "#/components/schemas/Dog", "Cat" -> "#/components/schemas/Cat")
              ))
            ),
            "Holder" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "pet" -> OpenAPISchema.OneOf(
                  schemas = List(
                    OpenAPISchema.Ref("#/components/schemas/Dog"),
                    OpenAPISchema.Ref("#/components/schemas/Cat")
                  )
                )
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()

      // Parent abstract class should be generated
      val parentFile = result.find(_.fileName == "animal.dart").get
      parentFile.source should include("abstract class Animal")
      parentFile.source should include("String get name;")
      parentFile.source should include("factory Animal.fromJson(Map<String, dynamic> json)")
      parentFile.source should include("Map<String, dynamic> toJson();")

      // Child classes extend parent
      val dogFile = result.find(_.fileName == "dog.dart").get
      dogFile.source should include("class Dog extends Animal with EquatableMixin")
      dogFile.source should include("import 'animal.dart';")

      val catFile = result.find(_.fileName == "cat.dart").get
      catFile.source should include("class Cat extends Animal with EquatableMixin")
    }

    "generate correct discriminator-based toJson for child classes" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Circle" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "radius" -> OpenAPISchema.Component(`type` = "number")
              )
            ),
            "Square" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "side" -> OpenAPISchema.Component(`type` = "number")
              )
            ),
            "Shape" -> OpenAPISchema.OneOf(
              schemas = List(
                OpenAPISchema.Ref("#/components/schemas/Circle"),
                OpenAPISchema.Ref("#/components/schemas/Square")
              ),
              discriminator = Some(OpenAPISchema.Discriminator(
                propertyName = "type",
                mapping = Map("Circle" -> "#/components/schemas/Circle", "Square" -> "#/components/schemas/Square")
              ))
            ),
            "Canvas" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "shape" -> OpenAPISchema.OneOf(
                  schemas = List(
                    OpenAPISchema.Ref("#/components/schemas/Circle"),
                    OpenAPISchema.Ref("#/components/schemas/Square")
                  )
                )
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()

      val circleFile = result.find(_.fileName == "circle.dart").get
      circleFile.source should include("map['type'] = 'Circle';")
      circleFile.source should include("return map;")

      val squareFile = result.find(_.fileName == "square.dart").get
      squareFile.source should include("map['type'] = 'Square';")
    }

    "filter out primitive-only schemas from file generation" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "PlainString" -> OpenAPISchema.Component(`type` = "string"),
            "PlainInt" -> OpenAPISchema.Component(`type` = "integer"),
            "PlainBool" -> OpenAPISchema.Component(`type` = "boolean"),
            "PlainNumber" -> OpenAPISchema.Component(`type` = "number"),
            "RealObject" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map("x" -> OpenAPISchema.Component(`type` = "string"))
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val modelFiles = result.filter(_.path == "lib/model")
      modelFiles.map(_.name).toSet should be(Set("RealObject"))
    }

    "not filter schemas with validation constraints" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "BoundedString" -> OpenAPISchema.Component(
              `type` = "string",
              minLength = Some(1),
              maxLength = Some(100)
            ),
            "BoundedInt" -> OpenAPISchema.Component(
              `type` = "integer",
              minimum = Some(num(0)),
              maximum = Some(num(999))
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      // Schemas with validation constraints should NOT be filtered out
      val modelFiles = result.filter(_.path == "lib/model")
      modelFiles.map(_.name).toSet should be(Set("BoundedString", "BoundedInt"))
    }

    "filter out schemas that conflict with Dart built-in types" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "string" -> OpenAPISchema.Component(`type` = "string", description = Some("test")),
            "List" -> OpenAPISchema.Component(`type` = "object", properties = Map("x" -> OpenAPISchema.Component(`type` = "string"))),
            "Map" -> OpenAPISchema.Component(`type` = "object", properties = Map("x" -> OpenAPISchema.Component(`type` = "string"))),
            "RealType" -> OpenAPISchema.Component(`type` = "object", properties = Map("x" -> OpenAPISchema.Component(`type` = "string")))
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val modelFiles = result.filter(_.path == "lib/model")
      modelFiles.map(_.name).toSet should be(Set("RealType"))
    }

    "handle renamed fields (bool -> b, _id -> id)" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Record" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "_id" -> OpenAPISchema.Component(`type` = "string"),
                "bool" -> OpenAPISchema.Component(`type` = "boolean")
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val recordFile = result.find(_.fileName == "record.dart").get
      recordFile.source should include("@JsonKey(name: '_id') final String id;")
      recordFile.source should include("@JsonKey(name: 'bool') final bool b;")
    }

    "generate the full server integration test unchanged" in {
      // This mirrors the existing OpenAPIHttpServerSpec server - lock in its Dart output.
      // xFullClass with nested class names (e.g., "spec.OpenAPIHttpServerSpec.Auth")
      // causes typeNameForComponent to produce "OpenAPIHttpServerSpecAuth" because
      // the penultimate segment starts with uppercase.
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Example Server", version = "1.0"),
        paths = Map(
          "/reverse" -> OpenAPIPath(
            methods = Map(
              HttpMethod.Post -> OpenAPIPathEntry(
                summary = "Reverses text",
                description = "Reverses text",
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "OK",
                    content = Some(OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Ref("#/components/schemas/ReverseResponse")
                      )
                    ))
                  )
                ),
                requestBody = Some(OpenAPIRequestBody(
                  required = true,
                  content = OpenAPIContent(
                    ContentType.`application/json` -> OpenAPIContentType(
                      schema = OpenAPISchema.Ref("#/components/schemas/ReverseRequest")
                    )
                  )
                ))
              )
            )
          )
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Auth" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "username" -> OpenAPISchema.Component(`type` = "string"),
                "password" -> OpenAPISchema.Component(`type` = "string")
              ),
              xFullClass = Some("spec.OpenAPIHttpServerSpec.Auth")
            ),
            "ReverseRequest" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "auth" -> OpenAPISchema.Ref("#/components/schemas/Auth"),
                "text" -> OpenAPISchema.Component(`type` = "string")
              ),
              xFullClass = Some("spec.OpenAPIHttpServerSpec.ReverseRequest")
            ),
            "ReverseResponse" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "text" -> OpenAPISchema.Component(`type` = "string", nullable = Some(true)),
                "error" -> OpenAPISchema.Component(`type` = "string", nullable = Some(true))
              ),
              xFullClass = Some("spec.OpenAPIHttpServerSpec.ReverseResponse")
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()

      // xFullClass "spec.OpenAPIHttpServerSpec.Auth" -> type name "OpenAPIHttpServerSpecAuth"
      // because penultimate segment "OpenAPIHttpServerSpec" starts with uppercase
      result.map(_.fileName).toSet should be(Set(
        "service.dart",
        "open_a_p_i_http_server_spec_auth.dart",
        "open_a_p_i_http_server_spec_reverse_request.dart",
        "open_a_p_i_http_server_spec_reverse_response.dart"
      ))

      val authFile = result.find(_.name == "OpenAPIHttpServerSpecAuth").get
      authFile.source should include("class OpenAPIHttpServerSpecAuth extends Equatable")
      authFile.source should include("final String username;")
      authFile.source should include("final String password;")

      val reqFile = result.find(_.name == "OpenAPIHttpServerSpecReverseRequest").get
      reqFile.source should include("final OpenAPIHttpServerSpecAuth auth;")
      reqFile.source should include("final String text;")

      val resFile = result.find(_.name == "OpenAPIHttpServerSpecReverseResponse").get
      resFile.source should include("final String? text;")
      resFile.source should include("final String? error;")
    }

    "generate correct YAML output format" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test API", version = "1.0"),
        paths = Map(
          "/hello" -> OpenAPIPath(
            methods = Map(
              HttpMethod.Get -> OpenAPIPathEntry(
                summary = "Say hello",
                description = "Returns a greeting",
                responses = Map(
                  "200" -> OpenAPIResponse(
                    description = "OK",
                    content = Some(OpenAPIContent(
                      ContentType.`application/json` -> OpenAPIContentType(
                        schema = OpenAPISchema.Component(`type` = "string")
                      )
                    ))
                  )
                )
              )
            )
          )
        )
      )
      val yaml = api.asYaml
      yaml should include("openapi: '3.2.0'")
      yaml should include("title: 'Test API'")
      yaml should include("version: '1.0'")
    }

    "generate correct JSON output format" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test API", version = "1.0")
      )
      val json = api.asJson
      json("openapi").asString should be("3.2.0")
      json("info")("title").asString should be("Test API")
      json("info")("version").asString should be("1.0")
    }

    "handle json type mapping to Map<String, dynamic>" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Flexible" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "data" -> OpenAPISchema.Component(`type` = "json")
              )
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val flexFile = result.find(_.fileName == "flexible.dart").get
      flexFile.source should include("final Map<String, dynamic> data;")
    }

    "generate service file structure correctly" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        paths = Map.empty,
        components = Some(OpenAPIComponents(schemas = Map.empty))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val serviceFile = result.find(_.fileName == "service.dart").get
      serviceFile.path should be("lib")
      serviceFile.language should be("Dart")
      serviceFile.name should be("Service")
      serviceFile.source should include("class Service")
      serviceFile.source should include("static Uri base = Uri.base;")
    }

    "generate model file structure correctly" in {
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Foo" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map("x" -> OpenAPISchema.Component(`type` = "string"))
            )
          )
        ))
      )
      val generator = OpenAPIDartGenerator(api, OpenAPIGeneratorConfig())
      val result = generator.generate()
      val fooFile = result.find(_.fileName == "foo.dart").get
      fooFile.path should be("lib/model")
      fooFile.language should be("Dart")
      fooFile.name should be("Foo")
    }

    "auto-infer parent-child relationships from OneOf component schemas" in {
      // Register "Animal" as a OneOf component with $ref children — no manual baseNames needed
      val api = OpenAPI(
        info = OpenAPIInfo(title = "Test", version = "1.0"),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Dog" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "breed" -> OpenAPISchema.Component(`type` = "string")
              )
            ),
            "Cat" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "color" -> OpenAPISchema.Component(`type` = "string")
              )
            ),
            "Animal" -> OpenAPISchema.OneOf(
              schemas = List(
                OpenAPISchema.Ref("#/components/schemas/Dog"),
                OpenAPISchema.Ref("#/components/schemas/Cat")
              ),
              discriminator = Some(OpenAPISchema.Discriminator(
                propertyName = "type",
                mapping = Map(
                  "Dog" -> "#/components/schemas/Dog",
                  "Cat" -> "#/components/schemas/Cat"
                )
              ))
            ),
            "Holder" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "pet" -> OpenAPISchema.OneOf(
                  schemas = List(
                    OpenAPISchema.Ref("#/components/schemas/Dog"),
                    OpenAPISchema.Ref("#/components/schemas/Cat")
                  )
                )
              )
            )
          )
        ))
      )
      // No baseNames specified — auto-inferred from "Animal" OneOf component
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()

      // Parent abstract class should be generated
      val parentFile = result.find(_.fileName == "animal.dart").get
      parentFile.source should include("abstract class Animal")
      parentFile.source should include("String get name;")
      parentFile.source should include("factory Animal.fromJson(Map<String, dynamic> json)")

      // Child classes extend parent
      val dogFile = result.find(_.fileName == "dog.dart").get
      dogFile.source should include("class Dog extends Animal with EquatableMixin")

      val catFile = result.find(_.fileName == "cat.dart").get
      catFile.source should include("class Cat extends Animal with EquatableMixin")
    }
  }
}
