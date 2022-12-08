package spice.http.server.openapi

import fabric.Json
import fabric.define.DefType
import fabric.rw._

case class OpenAPIComponentSchema(`type`: String,
                                  description: Option[String] = None,
                                  maxLength: Option[Int] = None,
                                  minimum: Option[Int] = None,
                                  maximum: Option[Int] = None,
                                  example: Option[Json] = None,
                                  `enum`: List[Json] = Nil,
                                  maxItems: Option[Int] = None,
                                  minItems: Option[Int] = None,
                                  items: Option[Either[OpenAPIComponentSchema, OpenAPISchema]] = None,
                                  properties: Map[String, Either[OpenAPIComponentSchema, OpenAPISchema]] = Map.empty)

object OpenAPIComponentSchema {
  implicit val itemRW: RW[Either[OpenAPIComponentSchema, OpenAPISchema]] = new RW[Either[OpenAPIComponentSchema, OpenAPISchema]] {
    override def write(json: Json): Either[OpenAPIComponentSchema, OpenAPISchema] = json.get("type") match {
      case Some(_) => Left(json.as[OpenAPIComponentSchema])
      case None => Right(json.as[OpenAPISchema])
    }

    override def read(t: Either[OpenAPIComponentSchema, OpenAPISchema]): Json = t match {
      case Left(schema) => schema.json
      case Right(schema) => schema.json
    }

    override def definition: DefType = OpenAPIComponentSchema.rw.definition.merge(OpenAPISchema.rw.definition)
  }
  implicit val rw: RW[OpenAPIComponentSchema] = RW.gen
}