// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

/**
  * A change object represents a future or past change in a streaming catalog.
  * It contains the details such as the type of the change
  * (could be past change such as like new, updated, removed;
  * or a future change such as expiring, upcoming),
  * the affected item type (show, season or episode), timestamp of the change and more.
  * 
  * Via change endpoints, you can get the most recent updates in the streaming catalogs.
  * On top of the changes, you can also get the details of the affected shows. Every change object
  * has a showId field.
  * You can find the list of shows affected by the changes in the shows field of the response, and match
  * the show ids with the showId field of the change objects.
  */
case class Change(changeType: ChangeType,
                 itemType: ItemType,
                 showId: String,
                 showType: ShowType,
                 service: ServiceInfo,
                 streamingOptionType: StreamingOptionType,
                 season: Option[Int] = None,
                 episode: Option[Int] = None,
                 addon: Option[Addon] = None,
                 timestamp: Option[Long] = None,
                 link: Option[String] = None) derives RW
