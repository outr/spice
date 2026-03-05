package spice.openapi.generator

/**
 * Configuration for generating code for OpenAPI
 *
 * @param baseNames the base names for polymorphic types since this information is lost in OpenAPI. For example:
 *                  "Animal" -> Set("Dog", "Cat")
 * @param discriminatorMappings maps class names to their discriminator values when they differ.
 *                              For example: Map("Line" -> "LineString", "MultiLine" -> "MultiLineString")
 */
case class OpenAPIGeneratorConfig(baseNames: (String, Set[String])*) {
  private var _discriminatorMappings: Map[String, String] = Map.empty

  def withDiscriminatorMappings(mappings: Map[String, String]): this.type = {
    _discriminatorMappings = mappings
    this
  }

  lazy val baseForTypeMap: Map[String, String] = baseNames.flatMap {
    case (parent, children) => children.toList.map { child =>
      child -> parent
    } ::: children.toList.map { child =>
      s"$parent$child" -> parent
    }
  }.toMap

  def discriminatorValue(className: String): String =
    _discriminatorMappings.getOrElse(className, className)
}
