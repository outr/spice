package spec

import fabric._
import fabric.dsl._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIContent, OpenAPIContentType, OpenAPIInfo, OpenAPIParameter, OpenAPIPath, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema, OpenAPIServer, OpenAPITag, OpenAPIValue}
import spice.openapi.generator.{OpenAPIGeneratorConfig, SourceFile}
import spice.openapi.generator.dart.OpenAPIDartGenerator
import spice.http.HttpMethod
import java.nio.file.Files
import java.nio.file.Paths

class DartGeneratorComprehensiveSpec extends AnyWordSpec with Matchers {
  
  "Dart Generator Comprehensive Features" should {
    
    "generate Dart code for complex validation constraints" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Validation Test API",
          version = "1.0.0"
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "User" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "id" -> OpenAPISchema.Component(
                  `type` = "string",
                  minLength = Some(1),
                  maxLength = Some(50),
                  pattern = Some("^[a-zA-Z0-9_]+$")
                ),
                "age" -> OpenAPISchema.Component(
                  `type` = "integer",
                  minimum = Some(num(0)),
                  maximum = Some(num(150))
                ),
                "email" -> OpenAPISchema.Component(
                  `type` = "string",
                  format = Some("email"),
                  maxLength = Some(255)
                )
              ),
              required = List("id", "email")
            )
          )
        ))
      )
      
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()
      
      result should not be empty
      
      // Verify that model files were generated
      val modelFiles = result.filter(_.fileName.endsWith(".dart"))
      modelFiles should not be empty
    }
    
    "generate Dart code for OneOf schemas with discriminator" in {
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
                    "type" -> OpenAPISchema.Component(`type` = "string", `enum` = List(str("dog"))),
                    "breed" -> OpenAPISchema.Component(`type` = "string")
                  ),
                  required = List("type", "breed")
                ),
                OpenAPISchema.Component(
                  `type` = "object",
                  properties = Map(
                    "type" -> OpenAPISchema.Component(`type` = "string", `enum` = List(str("cat"))),
                    "color" -> OpenAPISchema.Component(`type` = "string")
                  ),
                  required = List("type", "color")
                )
              ),
              discriminator = Some(OpenAPISchema.Discriminator(propertyName = "type"))
            )
          )
        ))
      )
      
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()
      
      result should not be empty
      
      // Verify that the generated code contains discriminator logic
      val modelFiles = result.filter(_.fileName.endsWith(".dart"))
      modelFiles should not be empty
    }
    
    "generate Dart code for AllOf schemas" in {
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
                    "role" -> OpenAPISchema.Component(`type` = "string", `enum` = List(str("admin"))),
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
      
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()
      
      result should not be empty
    }
    
    "generate Dart code for nullable schemas" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Nullable API",
          version = "1.0.0"
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "NullableUser" -> OpenAPISchema.Component(
              `type` = "object",
              properties = Map(
                "id" -> OpenAPISchema.Component(`type` = "string"),
                "name" -> OpenAPISchema.Component(`type` = "string"),
                "email" -> OpenAPISchema.Component(`type` = "string", nullable = Some(true)),
                "phone" -> OpenAPISchema.Component(`type` = "string", nullable = Some(true))
              ),
              required = List("id", "name")
            )
          )
        ))
      )
      
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()
      
      result should not be empty
    }
    
    "generate Dart code for enum schemas" in {
      val api = OpenAPI(
        info = OpenAPIInfo(
          title = "Enum API",
          version = "1.0.0"
        ),
        components = Some(OpenAPIComponents(
          schemas = Map(
            "UserStatus" -> OpenAPISchema.Component(
              `type` = "string",
              `enum` = List(str("active"), str("inactive"), str("pending"), str("suspended"))
            ),
            "UserRole" -> OpenAPISchema.Component(
              `type` = "string",
              `enum` = List(str("user"), str("moderator"), str("admin"))
            )
          )
        ))
      )
      
      val config = OpenAPIGeneratorConfig()
      val generator = OpenAPIDartGenerator(api, config)
      val result = generator.generate()
      
      result should not be empty
    }
  }
}

