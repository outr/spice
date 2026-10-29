package spice.http.durable

import org.scalajs.dom
import rapid.{Stream, Task}
import spice.http.Headers

import scala.scalajs.js
import scala.scalajs.js.typedarray.*

/**
 * A fully-received file transfer, held in memory (a browser has no file system to spool it to): `data` is its bytes,
 * `value` the typed app payload the sender attached, and `headers` the HTTP-semantic envelope (Content-Type,
 * Content-Length, filename via Content-Disposition).
 */
case class ReceivedFile[F](transferId: String,
                           value: F,
                           headers: Headers,
                           data: Array[Byte]) extends ReceivedFileHeaders {
  def bytes: Stream[Byte] = Stream.fromIterator(Task(data.iterator))

  /** Size in bytes of the received file. */
  def size: Long = data.length.toLong

  /** The bytes as a Blob of the declared type, to show (`URL.createObjectURL`) or save. */
  def blob: dom.Blob = new dom.Blob(js.Array(byteArray2Int8Array(data).buffer: dom.BlobPart),
    new dom.BlobPropertyBag { `type` = contentType.getOrElse("application/octet-stream") })
}
