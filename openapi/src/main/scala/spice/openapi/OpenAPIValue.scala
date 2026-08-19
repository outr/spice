package spice.openapi

import fabric.*
import fabric.rw.*

/**
 * An Example Object.
 *
 * Two things here exist only so this model can be READ as well as written, and both were found by
 * parsing a published third-party spec:
 *
 *  - the specification allows an example to be given EITHER literally (`value`) or by URL
 *    (`externalValue`), so neither can be required;
 *  - `value` is typed as ANY JSON, not a string. spice only ever emitted strings, so a spec whose
 *    example is an object (`{"message": "An error occurred."}`) could not be deserialized.
 *
 * A literal string still serializes as `{"value": "..."}`, so emitted specs are unchanged.
 */
case class OpenAPIValue(value: Option[Json] = None,
                        externalValue: Option[String] = None,
                        summary: Option[String] = None,
                        description: Option[String] = None) derives RW

object OpenAPIValue {
  /** Source-compatible with the original `OpenAPIValue(value: String)`. Widening the field is what
    * lets a third-party spec parse; this overload is what stops that from breaking every caller
    * already writing the literal form. */
  def apply(value: String): OpenAPIValue = OpenAPIValue(value = Some(str(value)))
}
