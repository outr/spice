// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * Price of the renting or buying the item.
  * 
  * A movie and an episode that is available to buy or rent will always have a price.
  * 
  * A series or a season that is available to buy or rent may have a price or not.
  * If the price is available, that means the entire series or the season can be bought or rented as a whole
  * for the given price.
  * If the price is null, that means sub-items of the series or the season are available to buy or rent,
  * but it is not possible to buy or rent the entire series or the season as a whole at once.
  * In this case, the price of the sub-items can be found in the episodes or seasons array.
  */
case class Price(amount: String,
                 currency: String,
                 formatted: String) derives RW
