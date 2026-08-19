package spice.openapi

import fabric.rw.*

/**
 * A response, either declared inline or referenced from `components/responses`.
 *
 * `ref` and the defaults on the other fields exist so this model can be READ as well as written.
 * spice always emits responses inline, so nothing here ever produced a `$ref` - but a third-party spec
 * routinely writes `{"$ref": "#/components/responses/foo"}`, and parsing one failed outright on the
 * missing `description`. Additive rather than a sealed hierarchy: every existing construction site and
 * every downstream caller keeps compiling, and `RemoveNullsFilter` drops the empty `ref` on output so
 * emitted specs are byte-identical to before.
 */
case class OpenAPIResponse(description: String = "",
                           content: Option[OpenAPIContent] = None,
                           ref: Option[String] = None) derives RW {
  /** True when this is a reference rather than an inline declaration. */
  def isRef: Boolean = ref.nonEmpty

  /** The referenced name (`countriesResponse` for `#/components/responses/countriesResponse`). */
  def refName: Option[String] = ref.map(r => r.substring(r.lastIndexOf('/') + 1))
}
