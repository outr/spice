package spice.openapi

import fabric.rw.*

/**
 * Represents a resolved type parameter for generic types in the OpenAPI model.
 * For example, `Id[User]` would have `OpenAPIGenericType("T", "User")`.
 *
 * @param name the type parameter name (e.g., "T")
 * @param typeName the resolved type name (e.g., "User")
 */
case class OpenAPIGenericType(name: String, typeName: String) derives RW
