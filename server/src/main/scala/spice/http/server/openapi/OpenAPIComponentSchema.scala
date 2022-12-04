package spice.http.server.openapi

import fabric.Json
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
                                  properties: Map[String, OpenAPISchema] = Map.empty)

object OpenAPIComponentSchema {
  implicit val itemRW: RW[Either[OpenAPIComponentSchema, OpenAPISchema]] = RW.from(
    r = {
      case Left(schema) => schema.json
      case Right(schema) => schema.json
    },
    w = json => json.get("type") match {
      case Some(_) => Left(json.as[OpenAPIComponentSchema])
      case None => Right(json.as[OpenAPISchema])
    }
  )
  implicit val rw: RW[OpenAPIComponentSchema] = RW.gen
}