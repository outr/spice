// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * A language and optionally an associated region.
  */
case class Locale(language: String,
                 region: Option[String] = None) derives RW
