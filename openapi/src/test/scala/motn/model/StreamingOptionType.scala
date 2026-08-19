// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Type of the streaming option.
  */
enum StreamingOptionType(val wire: String) {
  case Free extends StreamingOptionType("free")
  case Subscription extends StreamingOptionType("subscription")
  case Buy extends StreamingOptionType("buy")
  case Rent extends StreamingOptionType("rent")
  case Addon extends StreamingOptionType("addon")
}

object StreamingOptionType {
  given RW[StreamingOptionType] = RW.enumeration[StreamingOptionType](list = values.toList, asString = _.wire)

  def apply(wire: String): Option[StreamingOptionType] = values.find(_.wire == wire)
}
