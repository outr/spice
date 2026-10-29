package spice.http.durable

import spice.http.Headers

/**
 * Where a transfer's bytes are kept while they move: a temp file on the JVM, memory in a browser, which has no file
 * system to write to. Bytes go in and come out at a position, so chunks can arrive out of order and a resume can read
 * from wherever the receiver asks.
 */
trait FileSpool {
  /** Bytes held, up to the furthest written. */
  def size: Long

  def write(position: Long, bytes: Array[Byte]): Unit

  /** `length` bytes from `position`, fewer at the end. */
  def read(position: Long, length: Int): Array[Byte]

  /** Every byte has arrived: what was received, now the consumer's (on the JVM, the temp file is theirs to delete). */
  def received[F](transferId: String, value: F, headers: Headers): ReceivedFile[F]

  /** No longer needed (sent, aborted or timed out): let go of it, and delete it if this side made it. */
  def release(): Unit
}
