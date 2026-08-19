// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Type of an item.
  */
enum ItemType(val wire: String) {
  case Show extends ItemType("show")
  case Season extends ItemType("season")
  case Episode extends ItemType("episode")
}

object ItemType {
  given RW[ItemType] = RW.enumeration[ItemType](list = values.toList, asString = _.wire)

  def apply(wire: String): Option[ItemType] = values.find(_.wire == wire)
}
