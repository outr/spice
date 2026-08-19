// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Availability of the streaming option types in the service.
  */
case class StreamingOptionTypes(addon: Boolean,
                 buy: Boolean,
                 rent: Boolean,
                 free: Boolean,
                 subscription: Boolean) derives RW
