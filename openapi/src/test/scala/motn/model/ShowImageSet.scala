// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Image set of a show.
  */
case class ShowImageSet(verticalPoster: VerticalImage,
                 horizontalPoster: HorizontalImage,
                 verticalBackdrop: Option[VerticalImage] = None,
                 horizontalBackdrop: Option[HorizontalImage] = None) derives RW
