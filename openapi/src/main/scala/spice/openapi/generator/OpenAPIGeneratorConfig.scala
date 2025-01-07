package spice.openapi.generator

/**
 * Configuration for generating code for OpenAPI
 *
 * @param baseNames the base names for polymorphic types since this information is lost in OpenAPI. For example:
 *                  "Animal" -> Set("Dog", "Cat")
 */
case class OpenAPIGeneratorConfig(baseNames: (String, Set[String])*) {
  lazy val baseForTypeMap: Map[String, String] = baseNames.flatMap {
    case (parent, children) => children.toList.map { child =>
      child -> parent
    } ::: children.toList.map { child =>
      s"$parent$child" -> parent
    }
  }.toMap
}