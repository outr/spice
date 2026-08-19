// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * A streaming option.
  */
case class StreamingOption(service: ServiceInfo,
                 `type`: StreamingOptionType,
                 link: String,
                 audios: List[Locale],
                 subtitles: List[Subtitle],
                 expiresSoon: Boolean,
                 availableSince: Long,
                 addon: Option[Addon] = None,
                 videoLink: Option[String] = None,
                 quality: Option[String] = None,
                 price: Option[Price] = None,
                 expiresOn: Option[Long] = None) derives RW
