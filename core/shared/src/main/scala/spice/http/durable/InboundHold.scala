package spice.http.durable

/**
 * What a socket keeps of its peer's messages while nothing on this side listens for them yet, by kind (`events`,
 * `ephemeral`, `files`): a server session is active, and its client sending, before the application has attached its
 * handlers — which it may do only after resolving who the session is — and a message dispatched in that gap reached no
 * one. Each kind is held until [[release]] says it is heard, then delivered in the order it came; one not heard within
 * the socket's deadline, or past `limit` messages, is delivered as it would have been. Nothing is held unless
 * [[start]] is called.
 */
class InboundHold(limit: Int) {
  private var holding: Set[String] = Set.empty
  private var queues: Map[String, Vector[() => Unit]] = Map.empty

  /** Hold `kinds` from now. */
  def start(kinds: Set[String]): Unit = synchronized { holding = kinds }

  /** Whether `kind` is held. */
  def holds(kind: String): Boolean = synchronized(holding.contains(kind))

  /** Deliver a message of `kind` now, or keep it for when the kind is heard. */
  def apply(kind: String)(deliver: => Unit): Unit = {
    val now = synchronized {
      if (!holding.contains(kind)) true
      else {
        queues = queues.updated(kind, queues.getOrElse(kind, Vector.empty) :+ (() => deliver))
        false
      }
    }
    if (now) deliver
    else if (synchronized(queues.getOrElse(kind, Vector.empty).size > limit)) {
      scribe.warn(s"DurableSocket: over $limit $kind messages held with nothing listening; delivering them")
      release(kind)
    }
  }

  /** `kind` is heard: deliver what was held of it, in order, and hold no more. A message arriving while they are
    * delivered goes after them. */
  def release(kind: String): Int = {
    var delivered = 0
    var draining = true
    while (draining) {
      val batch = synchronized {
        val q = queues.getOrElse(kind, Vector.empty)
        queues = queues.removed(kind)
        if (q.isEmpty) holding = holding - kind
        q
      }
      if (batch.isEmpty) draining = false
      else batch.foreach { deliver =>
        delivered += 1
        try deliver() catch { case t: Throwable => scribe.warn(s"DurableSocket: a held $kind message failed", t) }
      }
    }
    delivered
  }

  /** Stop holding anything, delivering what was held; how many of each kind were held. */
  def releaseAll(): Map[String, Int] = synchronized(holding).toList.map(kind => kind -> release(kind)).toMap

  /** Stop holding and drop what was held (the socket closed). */
  def clear(): Unit = synchronized {
    holding = Set.empty
    queues = Map.empty
  }
}

object InboundHold {
  val Events: String = "events"
  val Ephemeral: String = "ephemeral"
  val Files: String = "files"
  val All: Set[String] = Set(Events, Ephemeral, Files)
}
