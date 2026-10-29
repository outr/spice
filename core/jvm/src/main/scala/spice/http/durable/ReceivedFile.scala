package spice.http.durable

import rapid.{Stream, Task}
import spice.http.Headers

import java.io.{BufferedInputStream, InputStream}
import java.nio.file.{Files, Path}

/**
 * A fully-received file transfer. The bytes have been spooled to `path` (a temp file the
 * receiver owns); `value` is the typed app payload the sender attached, and `headers` is the
 * HTTP-semantic envelope (Content-Type, Content-Length, filename via Content-Disposition).
 *
 * Consumers stream from `bytes` (lazy, never fully materialized) or read `path` directly. The
 * receiver is responsible for deleting `path` once the consumer is done with it.
 */
case class ReceivedFile[F](transferId: String,
                           value: F,
                           headers: Headers,
                           path: Path) extends ReceivedFileHeaders {
  /** Lazy byte stream over the spooled file. */
  def bytes: Stream[Byte] = Stream.fromIterator(Task {
    val input: InputStream = new BufferedInputStream(Files.newInputStream(path))
    Iterator.continually(input.read())
      .takeWhile(b => if (b == -1) { input.close(); false } else true)
      .map(_.toByte)
  })

  /** Size in bytes of the received file. */
  def size: Long = Files.size(path)
}
