// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Details of a streaming service localized according to the parent country.
  */
case class Service(id: String,
                 name: String,
                 homePage: String,
                 themeColorCode: String,
                 imageSet: ServiceImageSet,
                 streamingOptionTypes: StreamingOptionTypes,
                 addons: List[Addon]) derives RW
