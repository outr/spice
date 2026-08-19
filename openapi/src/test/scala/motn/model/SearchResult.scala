// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

case class SearchResult(shows: List[Show],
                 hasMore: Boolean,
                 nextCursor: Option[String] = None) derives RW
