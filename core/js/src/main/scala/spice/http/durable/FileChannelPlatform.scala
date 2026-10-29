package spice.http.durable

import org.scalajs.dom
import rapid.Task
import spice.http.Headers

import scala.scalajs.js
import scala.scalajs.js.typedarray.*

/** Sending what a browser holds: a picked or dropped `File`, or any `Blob`. */
trait FileChannelPlatform[F] {
  protected def sendSpool(name: String, contentType: String, spool: FileSpool, value: F, headers: Headers): Task[String]

  def send(file: dom.File, value: F, headers: Headers): Task[String] = send(file.name, file, value, headers)

  def send(file: dom.File, value: F): Task[String] = send(file, value, Headers.empty)

  /** `blob` as a file called `name`, its type taken from the headers, else the blob's own. */
  def send(name: String, blob: dom.Blob, value: F, headers: Headers): Task[String] = Task.defer {
    val read = Task.completable[Array[Byte]]
    blob.arrayBuffer().`then`[Unit](
      (buffer: ArrayBuffer) => { read.success(int8Array2ByteArray(new Int8Array(buffer))); () },
      js.defined((error: Any) => { read.failure(js.JavaScriptException(error)); () })
    )
    read.flatMap { bytes =>
      val ct = headers.first(Headers.`Content-Type`)
        .orElse(Option(blob.`type`).filter(_.nonEmpty))
        .getOrElse("application/octet-stream")
      sendSpool(name, ct, FileSpools.of(bytes), value, headers)
    }
  }

  def send(name: String, blob: dom.Blob, value: F): Task[String] = send(name, blob, value, Headers.empty)
}
