// GENERATED CODE: Do not edit!
package motn.model

import fabric.rw.*

case class ChangesResult(changes: List[Change],
                 shows: Map[String, Show],
                 hasMore: Boolean,
                 nextCursor: Option[String] = None) derives RW
