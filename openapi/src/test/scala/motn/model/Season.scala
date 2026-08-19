// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Details of a season.
  */
case class Season(itemType: String,
                 title: String,
                 firstAirYear: Int,
                 lastAirYear: Int,
                 streamingOptions: Map[String, List[StreamingOption]],
                 episodes: List[Episode] = Nil) derives RW
