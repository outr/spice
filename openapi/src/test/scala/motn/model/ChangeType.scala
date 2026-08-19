// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Type of the change.
  */
enum ChangeType(val wire: String) {
  case New extends ChangeType("new")
  case Removed extends ChangeType("removed")
  case Updated extends ChangeType("updated")
  case Expiring extends ChangeType("expiring")
  case Upcoming extends ChangeType("upcoming")
}

object ChangeType {
  given RW[ChangeType] = RW.enumeration[ChangeType](list = values.toList, asString = _.wire)

  def apply(wire: String): Option[ChangeType] = values.find(_.wire == wire)
}
