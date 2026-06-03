package spice.http.durable

import fabric.Json
import fabric.rw.*

/**
 * Control-plane messages for chunked binary file transfers. They ride the durable socket's
 * text channel (ephemeral, never logged); the file bytes themselves ride the binary channel
 * as framed chunks correlated by `transferId`. Mirrors the co-located style of [[EventMessage]]
 * and friends in `Protocol.scala`.
 *
 * Wire `type` discriminators: `file-start`, `file-end`, `file-ack`, `file-resume`, `file-abort`.
 */

/** Sender → receiver. Announces a transfer: intrinsic envelope (name/contentType/size), the
  * frame geometry (frameSize/totalChunks), the typed app payload `value`, and HTTP-semantic
  * `headers` (as a plain map — `Headers` has no RW). */
case class FileStartMessage(transferId: String,
                            name: String,
                            contentType: String,
                            size: Long,
                            frameSize: Int,
                            totalChunks: Int,
                            value: Json,
                            headers: Map[String, List[String]])
object FileStartMessage {
  given rw: RW[FileStartMessage] = RW.gen
}

/** Sender → receiver. All chunks for `transferId` have been sent; `totalBytes` lets the
  * receiver validate completeness before sealing. */
case class FileEndMessage(transferId: String, totalBytes: Long)
object FileEndMessage {
  given rw: RW[FileEndMessage] = RW.gen
}

/** Receiver → sender. Credit-window flow control: every chunk index `<= throughIndex` has
  * been durably written, so the sender may advance its window past it. */
case class FileAckMessage(transferId: String, throughIndex: Int)
object FileAckMessage {
  given rw: RW[FileAckMessage] = RW.gen
}

/** Receiver → sender, on reconnect. The receiver already holds chunks `[0, fromIndex)`;
  * the sender resumes from `fromIndex` rather than restarting. */
case class FileResumeMessage(transferId: String, fromIndex: Int)
object FileResumeMessage {
  given rw: RW[FileResumeMessage] = RW.gen
}

/** Either side. Abort `transferId`; the peer reaps its temp file and drops the transfer. */
case class FileAbortMessage(transferId: String, reason: String)
object FileAbortMessage {
  given rw: RW[FileAbortMessage] = RW.gen
}
