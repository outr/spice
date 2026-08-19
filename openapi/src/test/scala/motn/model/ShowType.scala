// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Type of a show.
  */
enum ShowType(val wire: String) {
  case Movie extends ShowType("movie")
  case Series extends ShowType("series")
}

object ShowType {
  given RW[ShowType] = RW.enumeration[ShowType](list = values.toList, asString = _.wire)

  def apply(wire: String): Option[ShowType] = values.find(_.wire == wire)
}
