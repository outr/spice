package spice.openapi

import fabric.rw.*

/**
 * A parameter, either declared inline or referenced from `components/parameters`.
 *
 * Defaults and `ref` for the same reason as [[OpenAPIResponse]]: spice emits parameters inline, but a
 * published spec references shared ones, and the required fields made such a spec unparseable.
 */
case class OpenAPIParameter(description: String = "",
                            name: String = "",
                            in: String = "query",
                            required: Boolean = false,
                            schema: OpenAPISchema = OpenAPISchema.Component("string"),
                            ref: Option[String] = None) derives RW {
  def isRef: Boolean = ref.nonEmpty
  def refName: Option[String] = ref.map(r => r.substring(r.lastIndexOf('/') + 1))
}
