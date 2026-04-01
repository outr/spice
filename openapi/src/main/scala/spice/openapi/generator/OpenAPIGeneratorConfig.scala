package spice.openapi.generator

import fabric.Str
import spice.openapi.{OpenAPI, OpenAPISchema}

/**
 * Configuration for generating code for OpenAPI
 */
case class OpenAPIGeneratorConfig() {
  private var _discriminatorMappings: Map[String, String] = Map.empty

  def withDiscriminatorMappings(mappings: Map[String, String]): this.type = {
    _discriminatorMappings = mappings
    this
  }

  /**
   * Builds the child-to-parent type map by auto-inferring relationships from the API spec:
   * - OneOf schemas with $ref children: child type -> parent type
   * - Enum component schemas: enum value -> enum type name
   */
  def buildBaseForTypeMap(api: OpenAPI): Map[String, String] = {
    val fromOneOf = buildBaseNames(api).flatMap { case (parent, children) =>
      children.toList.map(child => child -> parent) :::
        children.toList.map(child => s"$parent$child" -> parent)
    }.toMap

    val fromEnums = api.components.toList.flatMap { components =>
      components.schemas.flatMap {
        case (enumName, c: OpenAPISchema.Component) if c.`enum`.nonEmpty =>
          c.`enum`.collect { case Str(value, _) => value -> enumName }
        case _ => Nil
      }
    }.toMap

    fromOneOf ++ fromEnums
  }

  /**
   * Builds the parent-to-children map by auto-inferring from OneOf schemas in the API spec.
   */
  def buildBaseNames(api: OpenAPI): List[(String, Set[String])] = {
    api.components.toList.flatMap { components =>
      components.schemas.collect {
        case (parentName, oneOf: OpenAPISchema.OneOf) =>
          parentName -> oneOf.schemas.collect { case ref: OpenAPISchema.Ref => ref.name }.toSet
      }
    }
  }

  def discriminatorValue(className: String): String =
    _discriminatorMappings.getOrElse(className, className)
}
