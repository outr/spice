// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Countries are the primary way to get the supported streaming services and addons
  * (such as list of available Apple TV and Prime Video channels) in a region.
  * 
  * Each country object contains the country code, name and the list of supported streaming services.
  * 
  * Details of the streaming services include localized
  * logos, homepages, theme colors, and available streaming options and addons.
  */
case class Country(countryCode: String,
                 name: String,
                 services: List[Service]) derives RW
