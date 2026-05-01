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
   * Two distinct maps were historically merged here, which silently corrupts
   * class-hierarchy lookups when a case class happens to share its name with
   * a value of an unrelated enum (e.g. dw's `Flaring` case class and
   * `CaseType.Flaring` named enum value) — the merged map reports the enum
   * as the class's parent and the generator emits `class Flaring extends CaseType`,
   * which doesn't compile (Dart enums can't be extended).
   *
   * Keep them separate. Call sites that ask "what poly parent does this child
   * have?" use [[buildOneOfParentMap]]; call sites that ask "this discriminator
   * literal lives in which enum?" use [[buildEnumValueToTypeMap]].
   *
   * The legacy combined view is kept on this method for callers we haven't
   * audited yet — but new code should pick the right map directly.
   */
  def buildBaseForTypeMap(api: OpenAPI): Map[String, String] =
    buildEnumValueToTypeMap(api) ++ buildOneOfParentMap(api)

  /** OneOf children → their abstract parent (Dart class names). True class hierarchy. */
  def buildOneOfParentMap(api: OpenAPI): Map[String, String] =
    buildBaseNames(api).flatMap { case (parent, children) =>
      children.toList.map(child => child -> parent)
    }.toMap

  /** Enum literal value → owning enum's Dart type name. Used for discriminator lookups only. */
  def buildEnumValueToTypeMap(api: OpenAPI): Map[String, String] =
    api.components.toList.flatMap { components =>
      components.schemas.flatMap {
        case (enumName, c: OpenAPISchema.Component) if c.`enum`.nonEmpty =>
          c.`enum`.collect { case Str(value, _) => value -> dartName(enumName) }
        case _ => Nil
      }
    }.toMap

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
