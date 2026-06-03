package spice.http.durable

import java.nio.file.Path
import scala.concurrent.duration.*

/**
 * Tuning for chunked binary file transfers over a [[DurableSocket]].
 *
 * @param frameSize        bytes per binary frame; kept well under proxy websocket-message caps
 * @param windowChunks     credit window — how many unacked chunks the sender may have in flight before it pauses
 * @param ackEvery         the receiver emits a `file-ack` at least once every this many received chunks
 * @param transferTimeout  an in-flight transfer with no activity for this long is aborted and its temp file reaped
 * @param spoolDirectory   where inbound chunks and spooled stream sources are written; system temp dir when None
 */
case class FileTransferConfig(
  frameSize: Int = 64 * 1024,
  windowChunks: Int = 16,
  ackEvery: Int = 4,
  transferTimeout: FiniteDuration = 5.minutes,
  spoolDirectory: Option[Path] = None
)
