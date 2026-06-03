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
                           path: Path) {
  /** Lazy byte stream over the spooled file. Built from `Stream.fromIterator` (the shared
    * companion) rather than rapid's JVM-only `fromPath`, so this stays cross-compile-safe. */
  def bytes: Stream[Byte] = Stream.fromIterator(Task {
    val input: InputStream = new BufferedInputStream(Files.newInputStream(path))
    Iterator.continually(input.read())
      .takeWhile(b => if (b == -1) { input.close(); false } else true)
      .map(_.toByte)
  })

  /** Size in bytes of the received file. */
  def size: Long = Files.size(path)

  /** Declared content type, if the sender provided one. */
  def contentType: Option[String] = headers.first(Headers.`Content-Type`)

  /** Declared file name, if the sender provided one (Content-Disposition `filename`). */
  def fileName: Option[String] = headers.first(Headers.Response.`Content-Disposition`)
    .flatMap { v =>
      val marker = "filename="
      val i = v.indexOf(marker)
      if (i < 0) None else Some(v.substring(i + marker.length).trim.stripPrefix("\"").stripSuffix("\""))
    }
}

/** Progress pulse for an in-flight transfer (either direction). */
case class FileProgress(transferId: String, bytesSoFar: Long, totalBytes: Long) {
  /** Fraction complete in `[0, 1]`, or `0` when the total is unknown. */
  def fraction: Double = if (totalBytes <= 0L) 0.0 else bytesSoFar.toDouble / totalBytes.toDouble
}
