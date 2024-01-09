package spice.openapi

import fabric._
import fabric.define.DefType
import fabric.rw._
import spice.openapi.server.Schema

sealed trait OpenAPISchema {
  def makeNullable: OpenAPISchema
  def withSchema(schema: Schema): OpenAPISchema
}

object OpenAPISchema {
  case class Component(`type`: String,
                       nullable: Option[Boolean] = None,
                       description: Option[String] = None,
                       maxLength: Option[Int] = None,
                       minimum: Option[Int] = None,
                       maximum: Option[Int] = None,
                       example: Option[Json] = None,
                       `enum`: List[Json] = Nil,
                       format: Option[String] = None,
                       maxItems: Option[Int] = None,
                       minItems: Option[Int] = None,
                       items: Option[OpenAPISchema] = None,
                       properties: Map[String, OpenAPISchema] = Map.empty,
                       additionalProperties: Option[OpenAPISchema] = None) extends OpenAPISchema {
    override def makeNullable: OpenAPISchema = copy(nullable = Some(true))

    override def withSchema(schema: Schema): OpenAPISchema = copy(
      description = schema.description,
      maxLength = schema.maxLength,
      minimum = schema.minimum,
      maximum = schema.maximum,
      example = schema.example,
      maxItems = schema.maxItems,
      minItems = schema.minItems
    )
  }

  object Component {
    implicit val rw: RW[Component] = RW.gen
  }

  case class Ref(ref: String, nullable: Option[Boolean] = None) extends OpenAPISchema {
    override def makeNullable: OpenAPISchema = throw new UnsupportedOperationException("Ref cannot be made nullable")

    override def withSchema(schema: Schema): OpenAPISchema = this
  }

  object Ref {
    implicit val rw: RW[Ref] = RW.from(
      r = s => obj("$ref" -> s.ref, "nullable" -> s.nullable.json),
      w = j => Ref(j("$ref").asString, j.get("nullable").map(_.as[Boolean])),
      d = DefType.Obj(Some("Ref"), "$ref" -> DefType.Str, "nullable" -> DefType.Opt(DefType.Bool))
    )
  }

  trait MultiSchema extends OpenAPISchema {
    protected def modify(f: List[OpenAPISchema] => List[OpenAPISchema]): OpenAPISchema

    override def makeNullable: OpenAPISchema = modify(_.map(_.makeNullable))

    override def withSchema(schema: Schema): OpenAPISchema = modify(_.map(_.withSchema(schema)))
  }

  case class OneOf(schemas: List[OpenAPISchema],
                   discriminator: String = "type",
                   nullable: Option[Boolean] = None) extends MultiSchema {
    override protected def modify(f: List[OpenAPISchema] => List[OpenAPISchema]): OpenAPISchema = copy(f(schemas))
  }

  case class AllOf(schemas: List[OpenAPISchema],
                   discriminator: String = "type",
                   nullable: Option[Boolean] = None) extends MultiSchema {
    override protected def modify(f: List[OpenAPISchema] => List[OpenAPISchema]): OpenAPISchema = copy(f(schemas))
  }

  case class AnyOf(schemas: List[OpenAPISchema],
                   discriminator: String = "type",
                   nullable: Option[Boolean] = None) extends MultiSchema {
    override protected def modify(f: List[OpenAPISchema] => List[OpenAPISchema]): OpenAPISchema = copy(f(schemas))
  }

  private def multi(`type`: String,
                    schemas: List[OpenAPISchema],
                    propertyName: String,
                    nullable: Option[Boolean]): Json = obj(
    `type` -> schemas.json,
    "discriminator" -> obj(
      "propertyName" -> propertyName
    ),
    "nullable" -> nullable.json
  )

  implicit val rw: RW[OpenAPISchema] = RW.from[OpenAPISchema](
    r = {
      case s: Component => Component.rw.read(s)
      case s: Ref => Ref.rw.read(s)
      case OneOf(schemas, discriminator, nullable) => multi("oneOf", schemas, discriminator, nullable)
      case AllOf(schemas, discriminator, nullable) => multi("allOf", schemas, discriminator, nullable)
      case AnyOf(schemas, discriminator, nullable) => multi("anyOf", schemas, discriminator, nullable)
      case s => throw new UnsupportedOperationException(s"Unsupported schema: $s")
    },
    w = json => {
      def discriminator = json("discriminator").asString
      val n = json.get("nullable").map(_.asBoolean)
      if (json.get("$ref").nonEmpty) {
        Ref.rw.write(json)
      } else if (json.get("type").nonEmpty) {
        Component.rw.write(json)
      } else if (json.get("oneOf").nonEmpty) {
        OneOf(json("oneOf").as[List[OpenAPISchema]], discriminator, n)
      } else if (json.get("allOf").nonEmpty) {
        AllOf(json("allOf").as[List[OpenAPISchema]], discriminator, n)
      } else if (json.get("anyOf").nonEmpty) {
        AnyOf(json("anyOf").as[List[OpenAPISchema]], discriminator, n)
      } else {
        throw new RuntimeException(s"Unsupported OpenAPISchema: $json")
      }
    },
    d = DefType.Json
  )
}