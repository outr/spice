// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

enum OrderDirection(val wire: String) {
  case Asc extends OrderDirection("asc")
  case Desc extends OrderDirection("desc")
}

object OrderDirection {
  given RW[OrderDirection] = RW.enumeration[OrderDirection](list = values.toList, asString = _.wire)

  def apply(wire: String): Option[OrderDirection] = values.find(_.wire == wire)
}
