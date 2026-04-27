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

  /** Convert a full Scala className (e.g. "spec.OpenAPIHttpServerSpec.Auth") into a Dart class name
    * by concatenating the uppercase-leading class chain segments after the package. */
  private def dartName(cn: String): String = {
    val withoutTypeArgs = {
      val idx = cn.indexOf('[')
      if (idx == -1) cn else cn.substring(0, idx)
    }
    val parts = withoutTypeArgs.replace("$", ".").split('.').filter(_.nonEmpty).toList
    val classChain = parts.dropWhile(p => p.charAt(0).isLower)
    if (classChain.nonEmpty) classChain.mkString else parts.lastOption.getOrElse(cn)
  }

  /**
   * Builds the child-to-parent Dart type map by auto-inferring relationships from the API spec:
   * - OneOf schemas with $ref children: child Dart type -> parent Dart type
   * - Enum component schemas: enum value -> enum Dart type name
   */
  def buildBaseForTypeMap(api: OpenAPI): Map[String, String] = {
    val fromOneOf = buildBaseNames(api).flatMap { case (parent, children) =>
      children.toList.map(child => child -> parent)
    }.toMap

    val fromEnums = api.components.toList.flatMap { components =>
      components.schemas.flatMap {
        case (enumName, c: OpenAPISchema.Component) if c.`enum`.nonEmpty =>
          c.`enum`.collect { case Str(value, _) => value -> dartName(enumName) }
        case _ => Nil
      }
    }.toMap

    fromEnums ++ fromOneOf
  }

  /**
   * Builds the parent-to-children Dart-name map by auto-inferring from OneOf schemas in the API spec.
   * Both parent and children are returned as Dart class names.
   */
  def buildBaseNames(api: OpenAPI): List[(String, Set[String])] = {
    api.components.toList.flatMap { components =>
      components.schemas.collect {
        case (parentName, oneOf: OpenAPISchema.OneOf) =>
          dartName(parentName) -> oneOf.schemas.collect { case ref: OpenAPISchema.Ref => dartName(ref.name) }.toSet
      }
    }
  }

  def discriminatorValue(className: String): String =
    _discriminatorMappings.getOrElse(className, className)
}
