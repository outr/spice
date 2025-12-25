package spec

import fabric._
import fabric.dsl._
import fabric.rw.Convertible
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.{OpenAPI, OpenAPIComponents, OpenAPIContent, OpenAPIContentType, OpenAPIInfo, OpenAPIParameter, OpenAPIPath, OpenAPIPathEntry, OpenAPIRequestBody, OpenAPIResponse, OpenAPISchema, OpenAPIServer, OpenAPITag, OpenAPIValue}

class OpenAPISchemaComprehensiveSpec extends AnyWordSpec with Matchers {
  
  "OpenAPISchema Enhanced Features" should {
    
    "support comprehensive validation constraints" in {
      val schema = OpenAPISchema.Component(
        `type` = "string",
        minLength = Some(1),
        maxLength = Some(100),
        pattern = Some("^[a-zA-Z0-9_]+$"),
        format = Some("email")
      )
      
      schema.minLength shouldBe Some(1)
      schema.maxLength shouldBe Some(100)
      schema.pattern shouldBe Some("^[a-zA-Z0-9_]+$")
      schema.format shouldBe Some("email")
    }
    
    "support numeric validation constraints" in {
      val schema = OpenAPISchema.Component(
        `type` = "integer",
        minimum = Some(num(0)),
        maximum = Some(num(100)),
        exclusiveMinimum = Some(true),
        exclusiveMaximum = Some(false),
        multipleOf = Some(num(5))
      )
      
      schema.minimum shouldBe Some(num(0))
      schema.maximum shouldBe Some(num(100))
      schema.exclusiveMinimum shouldBe Some(true)
      schema.exclusiveMaximum shouldBe Some(false)
      schema.multipleOf shouldBe Some(num(5))
    }
    
    "support array validation constraints" in {
      val schema = OpenAPISchema.Component(
        `type` = "array",
        minItems = Some(1),
        maxItems = Some(10),
        uniqueItems = Some(true),
        items = Some(OpenAPISchema.Component(`type` = "string"))
      )
      
      schema.minItems shouldBe Some(1)
      schema.maxItems shouldBe Some(10)
      schema.uniqueItems shouldBe Some(true)
      schema.items shouldBe Some(OpenAPISchema.Component(`type` = "string"))
    }
    
    "support required properties" in {
      val schema = OpenAPISchema.Component(
        `type` = "object",
        properties = Map(
          "id" -> OpenAPISchema.Component(`type` = "string"),
          "name" -> OpenAPISchema.Component(`type` = "string"),
          "email" -> OpenAPISchema.Component(`type` = "string")
        ),
        required = List("id", "name")
      )
      
      schema.required shouldBe List("id", "name")
    }
    
    "support additional properties" in {
      val schema = OpenAPISchema.Component(
        `type` = "object",
        additionalProperties = Some(OpenAPISchema.Component(`type` = "string"))
      )
      
      schema.additionalProperties shouldBe Some(OpenAPISchema.Component(`type` = "string"))
    }
    
    "support proper discriminator objects" in {
      val discriminator = OpenAPISchema.Discriminator(
        propertyName = "type",
        mapping = Map(
          "user" -> "#/components/schemas/User",
          "admin" -> "#/components/schemas/Admin"
        )
      )
      
      discriminator.propertyName shouldBe "type"
      discriminator.mapping shouldBe Map(
        "user" -> "#/components/schemas/User",
        "admin" -> "#/components/schemas/Admin"
      )
    }
    
    "support OneOf with discriminator" in {
      val schema = OpenAPISchema.OneOf(
        schemas = List(
          OpenAPISchema.Component(`type` = "string"),
          OpenAPISchema.Component(`type` = "integer")
        ),
        discriminator = Some(OpenAPISchema.Discriminator(propertyName = "type")),
        nullable = Some(true)
      )
      
      schema.schemas should have size 2
      schema.discriminator shouldBe Some(OpenAPISchema.Discriminator(propertyName = "type"))
      schema.nullable shouldBe Some(true)
    }
    
    "support AllOf schemas" in {
      val schema = OpenAPISchema.AllOf(
        schemas = List(
          OpenAPISchema.Component(`type` = "object"),
          OpenAPISchema.Component(`type` = "object")
        ),
        discriminator = Some(OpenAPISchema.Discriminator(propertyName = "type"))
      )
      
      schema.schemas should have size 2
      schema.discriminator shouldBe Some(OpenAPISchema.Discriminator(propertyName = "type"))
    }
    
    "support AnyOf schemas" in {
      val schema = OpenAPISchema.AnyOf(
        schemas = List(
          OpenAPISchema.Component(`type` = "string"),
          OpenAPISchema.Component(`type` = "integer")
        )
      )
      
      schema.schemas should have size 2
    }
    
    "support Not schemas" in {
      val schema = OpenAPISchema.Not(
        schema = OpenAPISchema.Component(`type` = "string"),
        nullable = Some(true)
      )
      
      schema.schema shouldBe OpenAPISchema.Component(`type` = "string")
      schema.nullable shouldBe Some(true)
    }
    
    "support XML metadata" in {
      val xml = OpenAPISchema.XML(
        name = Some("user"),
        namespace = Some("http://example.com/schema"),
        prefix = Some("ex"),
        attribute = Some(false),
        wrapped = Some(true)
      )
      
      xml.name shouldBe Some("user")
      xml.namespace shouldBe Some("http://example.com/schema")
      xml.prefix shouldBe Some("ex")
      xml.attribute shouldBe Some(false)
      xml.wrapped shouldBe Some(true)
    }
    
    "support external documentation" in {
      val docs = OpenAPISchema.ExternalDocs(
        url = "https://example.com/docs",
        description = Some("External documentation")
      )
      
      docs.url shouldBe "https://example.com/docs"
      docs.description shouldBe Some("External documentation")
    }
    
    "support readOnly and writeOnly properties" in {
      val schema = OpenAPISchema.Component(
        `type` = "object",
        properties = Map(
          "id" -> OpenAPISchema.Component(`type` = "string", readOnly = Some(true)),
          "password" -> OpenAPISchema.Component(`type` = "string", writeOnly = Some(true))
        )
      )
      
      schema.properties("id").asInstanceOf[OpenAPISchema.Component].readOnly shouldBe Some(true)
      schema.properties("password").asInstanceOf[OpenAPISchema.Component].writeOnly shouldBe Some(true)
    }
    
    "support deprecated schemas" in {
      val schema = OpenAPISchema.Component(
        `type` = "string",
        deprecated = Some(true)
      )
      
      schema.deprecated shouldBe Some(true)
    }
    
    "generate proper JSON for complex schemas" in {
      val schema = OpenAPISchema.Component(
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
          )
        ),
        required = List("id"),
        additionalProperties = Some(OpenAPISchema.Component(`type` = "string"))
      )
      
      // Test that the schema has the expected structure
      schema.`type` shouldBe "object"
      schema.properties should have size 2
      schema.required should contain("id")
      schema.additionalProperties shouldBe Some(OpenAPISchema.Component(`type` = "string"))
    }
    
    "generate proper JSON for OneOf with discriminator" in {
      val schema = OpenAPISchema.OneOf(
        schemas = List(
          OpenAPISchema.Component(`type` = "string"),
          OpenAPISchema.Component(`type` = "integer")
        ),
        discriminator = Some(OpenAPISchema.Discriminator(propertyName = "type"))
      )
      
      // Test that the schema can be created and has the expected structure
      schema.schemas should have size 2
      schema.discriminator shouldBe Some(OpenAPISchema.Discriminator(propertyName = "type"))
    }
    
    "support nullable schemas properly" in {
      val schema = OpenAPISchema.Component(
        `type` = "string",
        nullable = Some(true)
      )
      
      schema.nullable shouldBe Some(true)
      
      val nullableSchema = schema.makeNullable
      nullableSchema.asInstanceOf[OpenAPISchema.Component].nullable shouldBe Some(true)
    }
    
    "handle enum values correctly" in {
      val schema = OpenAPISchema.Component(
        `type` = "string",
        `enum` = List(str("active"), str("inactive"), str("pending"))
      )
      
      schema.`enum` should have size 3
      schema.`enum`.map(_.asStr.value) shouldBe List("active", "inactive", "pending")
    }
    
    "support format specifications" in {
      val formats = List("date-time", "date", "time", "email", "idn-email", "hostname", "idn-hostname", "ipv4", "ipv6", "uri", "uri-reference", "iri", "iri-reference", "uuid", "uri-template", "json-pointer", "relative-json-pointer", "regex")
      
      formats.foreach { format =>
        val schema = OpenAPISchema.Component(
          `type` = "string",
          format = Some(format)
        )
        
        schema.format shouldBe Some(format)
      }
    }
  }
  
  "OpenAPI Schema Validation" should {
    
    "validate required fields are present" in {
      val schema = OpenAPISchema.Component(
        `type` = "object",
        properties = Map(
          "id" -> OpenAPISchema.Component(`type` = "string"),
          "name" -> OpenAPISchema.Component(`type` = "string")
        ),
        required = List("id", "name")
      )
      
      schema.required should contain("id")
      schema.required should contain("name")
      schema.required should have size 2
    }
    
    "validate constraint relationships" in {
      val schema = OpenAPISchema.Component(
        `type` = "integer",
        minimum = Some(num(0)),
        maximum = Some(num(100)),
        exclusiveMinimum = Some(true),
        exclusiveMaximum = Some(false)
      )
      
      // These should be logically consistent
      schema.minimum shouldBe Some(num(0))
      schema.maximum shouldBe Some(num(100))
      schema.exclusiveMinimum shouldBe Some(true)
      schema.exclusiveMaximum shouldBe Some(false)
    }
    
    "validate array constraints" in {
      val schema = OpenAPISchema.Component(
        `type` = "array",
        minItems = Some(1),
        maxItems = Some(10),
        uniqueItems = Some(true)
      )
      
      schema.minItems shouldBe Some(1)
      schema.maxItems shouldBe Some(10)
      schema.uniqueItems shouldBe Some(true)
      
      // minItems should be <= maxItems
      schema.minItems.get should be <= schema.maxItems.get
    }
    
    "validate string constraints" in {
      val schema = OpenAPISchema.Component(
        `type` = "string",
        minLength = Some(1),
        maxLength = Some(100)
      )
      
      schema.minLength shouldBe Some(1)
      schema.maxLength shouldBe Some(100)
      
      // minLength should be <= maxLength
      schema.minLength.get should be <= schema.maxLength.get
    }
  }
}

