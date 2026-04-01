package spice.openapi

import fabric.*
import fabric.filter.JsonFilter

/**
 * Post-serialization filter that transforms the internal OpenAPI model representation
 * to comply with OpenAPI 3.2.0 / JSON Schema 2020-12:
 *
 * 1. `nullable: true` + `type: "X"` -> `type: ["X", "null"]` (removes `nullable`)
 * 2. `xFullClass` -> `x-full-class` (proper extension naming)
 * 3. `type: "json"` -> removes type (free-form schema, equivalent to `{}` in JSON Schema)
 */
object OpenAPI32Filter extends JsonFilter {
  override def apply(value: Json, path: JsonPath): Option[Json] = value match {
    case o: Obj =>
      val original = o.value
      var map = original
      val isNullable = map.get("nullable").exists(_.asBoolean)
      if (isNullable) {
        map = map - "nullable"
        map.get("type") match {
          case Some(Str(t, _)) =>
            map = map + ("type" -> Arr(Vector(str(t), str("null"))))
          case Some(arr: Arr) if !arr.value.exists(_ == str("null")) =>
            map = map + ("type" -> Arr(arr.value :+ str("null")))
          case _ =>
        }
      }
      map.get("xFullClass").foreach { v =>
        map = (map - "xFullClass") + ("x-full-class" -> v)
      }
      map.get("type") match {
        case Some(Str("json", _)) => map = map - "type"
        case _ =>
      }
      if (map eq original) Some(value) else Some(Obj(map.toList*))
    case _ => Some(value)
  }
}
