package spice.openapi

import fabric._
import fabric.define.DefType
import fabric.rw._
import fabric.dsl._
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
                       minLength: Option[Int] = None,
                       pattern: Option[String] = None,
                       minimum: Option[Json] = None,
                       maximum: Option[Json] = None,
                       exclusiveMinimum: Option[Boolean] = None,
                       exclusiveMaximum: Option[Boolean] = None,
                       multipleOf: Option[Json] = None,
                       example: Option[Json] = None,
                       `enum`: List[Json] = Nil,
                       format: Option[String] = None,
                       maxItems: Option[Int] = None,
                       minItems: Option[Int] = None,
                       uniqueItems: Option[Boolean] = None,
                       items: Option[OpenAPISchema] = None,
                       properties: Map[String, OpenAPISchema] = Map.empty,
                       required: List[String] = Nil,
                       additionalProperties: Option[OpenAPISchema] = None,
                       discriminator: Option[Discriminator] = None,
                       readOnly: Option[Boolean] = None,
                       writeOnly: Option[Boolean] = None,
                       xml: Option[XML] = None,
                       externalDocs: Option[ExternalDocs] = None,
                       deprecated: Option[Boolean] = None,
                       xFullClass: Option[String] = None) extends OpenAPISchema {
    override def makeNullable: OpenAPISchema = copy(nullable = Some(true))

    override def withSchema(schema: Schema): OpenAPISchema = copy(
      description = schema.description,
      maxLength = schema.maxLength,
      minimum = schema.minimum.map(num),
      maximum = schema.maximum.map(num),
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
      r = s => obj("$ref" -> str(s.ref), "nullable" -> s.nullable.json),
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
                   discriminator: Option[Discriminator] = None,
                   nullable: Option[Boolean] = None) extends MultiSchema {
    override protected def modify(f: List[OpenAPISchema] => List[OpenAPISchema]): OpenAPISchema = copy(f(schemas))
  }

  case class AllOf(schemas: List[OpenAPISchema],
                   discriminator: Option[Discriminator] = None,
                   nullable: Option[Boolean] = None) extends MultiSchema {
    override protected def modify(f: List[OpenAPISchema] => List[OpenAPISchema]): OpenAPISchema = copy(f(schemas))
  }

  case class AnyOf(schemas: List[OpenAPISchema],
                   discriminator: Option[Discriminator] = None,
                   nullable: Option[Boolean] = None) extends MultiSchema {
    override protected def modify(f: List[OpenAPISchema] => List[OpenAPISchema]): OpenAPISchema = copy(f(schemas))
  }

  case class Not(schema: OpenAPISchema, nullable: Option[Boolean] = None) extends OpenAPISchema {
    override def makeNullable: OpenAPISchema = copy(nullable = Some(true))

    override def withSchema(schema: Schema): OpenAPISchema = this
  }

  case class Discriminator(propertyName: String,
                          mapping: Map[String, String] = Map.empty)

  object Discriminator {
    implicit val rw: RW[Discriminator] = RW.gen
  }

  case class XML(name: Option[String] = None,
                 namespace: Option[String] = None,
                 prefix: Option[String] = None,
                 attribute: Option[Boolean] = None,
                 wrapped: Option[Boolean] = None)

  object XML {
    implicit val rw: RW[XML] = RW.gen
  }

  case class ExternalDocs(url: String, description: Option[String] = None)

  object ExternalDocs {
    implicit val rw: RW[ExternalDocs] = RW.gen
  }

  private def multi(`type`: String,
                    schemas: List[OpenAPISchema],
                    discriminator: Option[Discriminator],
                    nullable: Option[Boolean]): Json = {
    val fields = List(
      `type` -> schemas.json,
      "nullable" -> nullable.json
    ) ++ discriminator.map(disc => "discriminator" -> disc.json).toList
    
    obj(fields*)
  }

  private def notSchema(schema: OpenAPISchema, nullable: Option[Boolean]): Json = obj(
    "not" -> schema.json,
    "nullable" -> nullable.json
  )

  implicit val rw: RW[OpenAPISchema] = RW.from[OpenAPISchema](
    r = {
      case s: Component => Component.rw.read(s)
      case s: Ref => Ref.rw.read(s)
      case OneOf(schemas, discriminator, nullable) => multi("oneOf", schemas, discriminator, nullable)
      case AllOf(schemas, discriminator, nullable) => multi("allOf", schemas, discriminator, nullable)
      case AnyOf(schemas, discriminator, nullable) => multi("anyOf", schemas, discriminator, nullable)
      case Not(schema, nullable) => notSchema(schema, nullable)
      case s => throw new UnsupportedOperationException(s"Unsupported schema: $s")
    },
    w = json => {
      val n = json.get("nullable").map(_.asBoolean)
      if (json.get("$ref").nonEmpty) {
        Ref.rw.write(json)
      } else if (json.get("type").nonEmpty) {
        Component.rw.write(json)
      } else if (json.get("oneOf").nonEmpty) {
        val disc = json.get("discriminator").map(_.as[Discriminator])
        OneOf(json("oneOf").as[List[OpenAPISchema]], disc, n)
      } else if (json.get("allOf").nonEmpty) {
        val disc = json.get("discriminator").map(_.as[Discriminator])
        AllOf(json("allOf").as[List[OpenAPISchema]], disc, n)
      } else if (json.get("anyOf").nonEmpty) {
        val disc = json.get("discriminator").map(_.as[Discriminator])
        AnyOf(json("anyOf").as[List[OpenAPISchema]], disc, n)
      } else if (json.get("not").nonEmpty) {
        Not(json("not").as[OpenAPISchema], n)
      } else {
        throw new RuntimeException(s"Unsupported OpenAPISchema: $json")
      }
    },
    d = DefType.Json
  )
}