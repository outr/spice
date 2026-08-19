// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Details of an episode.
  */
case class Episode(itemType: String,
                 title: String,
                 airYear: Int,
                 streamingOptions: Map[String, List[StreamingOption]],
                 overview: Option[String] = None) derives RW
