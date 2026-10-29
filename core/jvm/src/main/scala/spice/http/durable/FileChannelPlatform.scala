package spice.http.durable

import rapid.Task
import spice.http.Headers

import java.nio.file.{Files, Path}

/** Sending what is on the JVM's file system, read where it is rather than copied. */
trait FileChannelPlatform[F] {
  protected def sendSpool(name: String, contentType: String, spool: FileSpool, value: F, headers: Headers): Task[String]

  def send(path: Path, value: F, headers: Headers): Task[String] = Task.defer {
    val name = Option(path.getFileName).map(_.toString).getOrElse("file")
    val ct = headers.first(Headers.`Content-Type`)
      .orElse(Option(Files.probeContentType(path)))
      .getOrElse("application/octet-stream")
    sendSpool(name, ct, FileSpools.open(path), value, headers)
  }

  def send(path: Path, value: F): Task[String] = send(path, value, Headers.empty)

  def send(file: java.io.File, value: F, headers: Headers): Task[String] = send(file.toPath, value, headers)
  def send(file: java.io.File, value: F): Task[String] = send(file.toPath, value, Headers.empty)
}
