package spice.http.durable

import fabric.Json

/** A logged event longer than one frame ([[DurableSocketConfig.maxFrameChars]]), announced on
  * [[DurableSocket.onOversized]] as it is sent in chunks: its sequence number, its length on the wire in characters, and
  * the event as JSON, for an app that wants to see what made it large. */
final case class OversizedMessage(seq: Long, length: Int, data: Json)
