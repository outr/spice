package spec

import fabric._
import fabric.dsl._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIContent, OpenAPIContentType, OpenAPIInfo, OpenAPIParameter, OpenAPIPath, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema, OpenAPIServer, OpenAPITag, OpenAPIValue}
import spice.openapi.generator.dart.OpenAPIDartGenerator
import spice.openapi.generator.OpenAPIGeneratorConfig
import spice.http.HttpMethod

class OpenAPIStandardsComplianceSpec extends AnyWordSpec with Matchers {
  
  "OpenAPI Standards Compliance" should {
    
    "support enhanced schema validation constraints" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Enhanced Validation API",
          version = "1.0.0"
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "EnhancedUser" -> OpenAPISchema.Component(
              `type` = "object",
              description = Some("User with enhanced validation constraints"),
              properties = Map(
                "username" -> OpenAPISchema.Component(
                  `type` = "string",
                  minLength = Some(3),
                  maxLength = Some(50),
                  pattern = Some("^[a-zA-Z0-9_]+$"),
                  description = Some("Username must be 3-50 characters, alphanumeric and underscore only")
                ),
                "email" -> OpenAPISchema.Component(
                  `type` = "string",
                  format = Some("email"),
                  maxLength = Some(255),
                  description = Some("Valid email address")
                ),
                "age" -> OpenAPISchema.Component(
                  `type` = "integer",
                  minimum = Some(num(13)),
                  maximum = Some(num(120)),
                  exclusiveMinimum = Some(true),
                  exclusiveMaximum = Some(false),
                  multipleOf = Some(num(1)),
                  description = Some("Age must be between 13 and 120")
                ),
                "score" -> OpenAPISchema.Component(
                  `type` = "number",
                  minimum = Some(num(0.0)),
                  maximum = Some(num(100.0)),
                  exclusiveMinimum = Some(false),
                  exclusiveMaximum = Some(false),
                  multipleOf = Some(num(0.1)),
                  description = Some("Score must be between 0 and 100, with 0.1 precision")
                ),
                "tags" -> OpenAPISchema.Component(
                  `type` = "array",
                  items = Some(OpenAPISchema.Component(
                    `type` = "string",
                    minLength = Some(1),
                    maxLength = Some(20)
                  )),
                  minItems = Some(0),
                  maxItems = Some(10),
                  uniqueItems = Some(true),
                  description = Some("Up to 10 unique tags, each 1-20 characters")
                ),
                "metadata" -> OpenAPISchema.Component(
                  `type` = "object",
                  additionalProperties = Some(OpenAPISchema.Component(`type` = "string")),
                  description = Some("Additional user metadata")
                )
              ),
              required = List("username", "email", "age")
            )
          )
        ))
      )
      
      // Test that the validation constraints are properly serialized
      val yaml = api.asYaml
      yaml should include("minLength: 3")
      yaml should include("maxLength: 50")
      yaml should include("pattern: '^[a-zA-Z0-9_]+$'")
      yaml should include("format: 'email'")
      yaml should include("minimum: 13")
      yaml should include("maximum: 120")
      yaml should include("exclusiveMinimum: true")
      yaml should include("multipleOf: 0.1")
      yaml should include("minItems: 0")
      yaml should include("maxItems: 10")
      yaml should include("uniqueItems: true")
      
      // Test that the API can be used with the Dart generator
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()
      
      result should not be empty
      
      // Verify that model files were generated
      val modelFiles = result.filter(_.fileName.endsWith(".dart"))
      modelFiles should have size 2 // Service + EnhancedUser
    }
    
    "support OneOf schemas with discriminators" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Polymorphic API",
          version = "1.0.0"
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "Pet" -> OpenAPISchema.OneOf(
              schemas = List(
                OpenAPISchema.Component(
                  `type` = "object",
                  properties = Map(
                    "type" -> OpenAPISchema.Component(
                      `type` = "string",
                      `enum` = List(str("dog"))
                    ),
                    "name" -> OpenAPISchema.Component(`type` = "string"),
                    "breed" -> OpenAPISchema.Component(`type` = "string")
                  ),
                  required = List("type", "name", "breed")
                ),
                OpenAPISchema.Component(
                  `type` = "object",
                  properties = Map(
                    "type" -> OpenAPISchema.Component(
                      `type` = "string",
                      `enum` = List(str("cat"))
                    ),
                    "name" -> OpenAPISchema.Component(`type` = "string"),
                    "color" -> OpenAPISchema.Component(`type` = "string")
                  ),
                  required = List("type", "name", "color")
                )
              ),
              discriminator = Some(OpenAPISchema.Discriminator(propertyName = "type"))
            )
          )
        ))
      )
      
      // Test that the polymorphic schema can be serialized
      val yaml = api.asYaml
      yaml should include("discriminator:")
      yaml should include("propertyName: 'type'")
      yaml should include("oneOf:")
      
      // Test that the API can be used with the Dart generator
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()
      
      result should not be empty
    }
    
    "support AllOf schemas for composition" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Composition API",
          version = "1.0.0"
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "BaseUser" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "id" -> OpenAPISchema.Component(`type` = "string"),
                "name" -> OpenAPISchema.Component(`type` = "string")
              ),
              required = List("id", "name")
            ),
            "AdminUser" -> OpenAPISchema.AllOf(
              schemas = List(
                OpenAPISchema.Ref("#/components/schemas/BaseUser"),
                OpenAPISchema.Component(
                  `type` = "object",
                  properties = Map(
                    "role" -> OpenAPISchema.Component(
                      `type` = "string",
                      `enum` = List(str("admin"))
                    ),
                    "permissions" -> OpenAPISchema.Component(
                      `type` = "array",
                      items = Some(OpenAPISchema.Component(`type` = "string"))
                    )
                  ),
                  required = List("role", "permissions")
                )
              )
            )
          )
        ))
      )
      
      // Test that the composition schema can be serialized
      val yaml = api.asYaml
      yaml should include("allOf:")
      
      // Test that the API can be used with the Dart generator
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()
      
      result should not be empty
    }
  }
}

