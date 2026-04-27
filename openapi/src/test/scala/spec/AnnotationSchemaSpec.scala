package spec

import fabric.*
import fabric.define.{DefType, Definition, Format}
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.generator.dart.{DurableSocketDartConfig, DurableSocketDartGenerator}
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIInfo, OpenAPISchema}

/**
 * Tests that Fabric annotation-driven schema definitions flow through to both
 * the OpenAPI schema model and the DurableSocket Dart code generator.
 *
 * Fabric annotations exercised:
 *   - @minLength / @maxLength / @pattern on strings
 *   - @minimum / @maximum / @exclusiveMinimum / @exclusiveMaximum / @multipleOf on numerics
 *   - @minItems / @maxItems / @uniqueItems on arrays
 *   - @format(Format.Email), @format(Format.DateTime), @format(Format.Uuid), etc.
 *   - @fieldDeprecated on fields
 *   - @description on fields
 */
class AnnotationSchemaSpec extends AnyWordSpec with Matchers {

  // --- Test types using Fabric annotations ---

  case class ValidatedUser(
    @minLength(3) @maxLength(50) @pattern("^[a-z]+$") username: String,
    @format(Format.Email) email: String,
    @minimum(0) @maximum(150) age: Int,
    @multipleOf(0.01) price: Double,
    @minItems(1) @maxItems(10) @uniqueItems(true) tags: List[String],
    @fieldDeprecated oldField: String,
    @description("The user's display name") displayName: String
  ) derives RW

  case class TimestampedEvent(
    name: String,
    @format(Format.Date) createdAt: String,
    @format(Format.Uuid) id: String
  ) derives RW

  // --- OpenAPI schema tests ---

  "OpenAPI schema generation from Fabric annotations" should {
    "convert validation constraints from Definition to Component fields" in {
      val d = summon[RW[ValidatedUser]].definition
      // Build schema via ServiceCall's schemaFrom → we'll use a minimal approach:
      // access the Obj fields and verify they carry annotations
      val obj = d.defType.asInstanceOf[DefType.Obj]
      val usernameField = obj.map("username")
      usernameField.constraints.minLength should be(Some(3))
      usernameField.constraints.maxLength should be(Some(50))
      usernameField.constraints.pattern should be(Some("^[a-z]+$"))

      val ageField = obj.map("age")
      ageField.constraints.minimum should be(Some(0.0))
      ageField.constraints.maximum should be(Some(150.0))

      val priceField = obj.map("price")
      priceField.constraints.multipleOf should be(Some(0.01))

      val tagsField = obj.map("tags")
      tagsField.constraints.minItems should be(Some(1))
      tagsField.constraints.maxItems should be(Some(10))
      tagsField.constraints.uniqueItems should be(Some(true))

      val emailField = obj.map("email")
      emailField.format should be(Format.Email)

      val oldField = obj.map("oldField")
      oldField.deprecated should be(true)

      val displayNameField = obj.map("displayName")
      displayNameField.description should be(Some("The user's display name"))
    }

    "propagate constraints to OpenAPISchema.Component via withDefinition" in {
      val d = summon[RW[ValidatedUser]].definition
      val obj = d.defType.asInstanceOf[DefType.Obj]

      // String with pattern/minLength/maxLength
      val usernameComponent = OpenAPISchema.Component(`type` = "string").withDefinition(obj.map("username"))
      usernameComponent.minLength should be(Some(3))
      usernameComponent.maxLength should be(Some(50))
      usernameComponent.pattern should be(Some("^[a-z]+$"))

      // Integer with min/max
      val ageComponent = OpenAPISchema.Component(`type` = "integer").withDefinition(obj.map("age"))
      ageComponent.minimum.map(_.asDouble) should be(Some(0.0))
      ageComponent.maximum.map(_.asDouble) should be(Some(150.0))

      // Number with multipleOf
      val priceComponent = OpenAPISchema.Component(`type` = "number").withDefinition(obj.map("price"))
      priceComponent.multipleOf.map(_.asDouble) should be(Some(0.01))

      // Array with minItems/maxItems/uniqueItems
      val tagsComponent = OpenAPISchema.Component(`type` = "array").withDefinition(obj.map("tags"))
      tagsComponent.minItems should be(Some(1))
      tagsComponent.maxItems should be(Some(10))
      tagsComponent.uniqueItems should be(Some(true))

      // Format.Email
      val emailComponent = OpenAPISchema.Component(`type` = "string").withDefinition(obj.map("email"))
      emailComponent.format should be(Some("email"))

      // Deprecated
      val oldComponent = OpenAPISchema.Component(`type` = "string").withDefinition(obj.map("oldField"))
      oldComponent.deprecated should be(Some(true))

      // Description
      val displayComponent = OpenAPISchema.Component(`type` = "string").withDefinition(obj.map("displayName"))
      displayComponent.description should be(Some("The user's display name"))
    }
  }

  // --- DurableSocket Dart tests ---

  "DurableSocket Dart generation from Fabric annotations" should {
    "propagate @format annotations through to the Definition" in {
      val d = summon[RW[TimestampedEvent]].definition
      val obj = d.defType.asInstanceOf[DefType.Obj]
      obj.map("createdAt").format should be(Format.Date)
      obj.map("id").format should be(Format.Uuid)
      obj.map("name").format should be(Format.Raw)
    }

    "emit @Deprecated annotation for @fieldDeprecated fields" in {
      val d = summon[RW[ValidatedUser]].definition
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        wireType = "ValidatedUser" -> d
      )
      val files = DurableSocketDartGenerator(config).generate()
      val source = files.find(_.fileName == "validated_user.dart").get.source

      source should include("@Deprecated(")
      source should include("oldField")
    }

    "emit doc comments for @description fields" in {
      val d = summon[RW[ValidatedUser]].definition
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        wireType = "ValidatedUser" -> d
      )
      val files = DurableSocketDartGenerator(config).generate()
      val source = files.find(_.fileName == "validated_user.dart").get.source

      source should include("/// The user's display name")
      source should include("final String displayName;")
    }
  }
}
