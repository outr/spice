package spice.openapi.server

import fabric.Json

case class Schema(description: Option[String] = None,
                  maxLength: Option[Int] = None,
                  minimum: Option[Int] = None,
                  maximum: Option[Int] = None,
                  example: Option[Json] = None,
                  maxItems: Option[Int] = None,
                  minItems: Option[Int] = None,
                  items: Option[Schema] = None,
                  properties: Map[String, Schema] = Map.empty)