package spice.http.content

import rapid.Task
import spice.net.ContentType
import spice.streamer.*
import spice.streamer.given

import java.net.URL
import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.util.Try

/** Content backed by a `java.net.URL` (typically a classloader resource — `jar:file:...!/foo`,
  * but works for `file:`, `http:`, etc. too).
  *
  * Length resolution: in the happy path `URLConnection.getContentLengthLong` returns the
  * resource's exact byte count and we serve it streamed. For some `JarURLConnection`
  * variants (sbt-native-packager Docker images, certain JVM/jar configurations) that
  * call returns `-1` — meaning "length unknown". Historically `URLContent.length` exposed
  * that `-1` directly, which produced `Content-Length: -1` on the wire. HTTP/1.1 frontends
  * tolerated it; HTTP/2 frontends (Cloud Run's GFE, Envoy-based proxies, anything strict)
  * reject the response framing entirely with no error visible to the application — the
  * client sees a 502.
  *
  * Fallback: when the connection returns `-1`, eagerly read the resource into memory once
  * and serve from the buffer. `length` reports the actual byte count, and `asString` /
  * `asStream` reuse the cached bytes so we never read the URL twice. This costs the
  * memory of one resource read — the same amount every consumer ends up paying when they
  * work around the bug by hand-rolling a `BytesContent` handler. */
case class URLContent(url: URL, contentType: ContentType, lastModifiedOverride: Option[Long] = None) extends Content {
  assert(url != null, "URL must not be null.")

  override def withContentType(contentType: ContentType): Content = copy(contentType = contentType)
  override def withLastModified(lastModified: Long): Content = copy(lastModifiedOverride = Some(lastModified))

  /** Resolved metadata + optional eager-loaded bytes.
    *
    * `cachedBytes` is `Some(...)` only on the fallback path (when `getContentLengthLong`
    * returned `-1`). On the happy path it's `None` and we keep streaming from the URL on
    * demand. Lazy so a caller that never asks for length doesn't pay for the resolve. */
  private case class Resolved(length: Long, lastModified: Long, cachedBytes: Option[Array[Byte]])

  private lazy val resolved: Resolved = {
    val connection = url.openConnection()
    val rawLength = connection.getContentLengthLong
    val rawModified = connection.getLastModified
    if (rawLength >= 0L) {
      // Length known — close the (possibly opened) input stream and serve streamed.
      Try(connection.getInputStream.close())
      Resolved(rawLength, rawModified, None)
    } else {
      // Length unknown — drain the stream once and cache. Read via getInputStream off
      // the same connection so we don't pay the URL resolve twice.
      val bytes = {
        val in = connection.getInputStream
        try in.readAllBytes()
        finally in.close()
      }
      Resolved(bytes.length.toLong, rawModified, Some(bytes))
    }
  }

  override def length: Long = resolved.length

  override def lastModified: Long = lastModifiedOverride.getOrElse(resolved.lastModified)

  override def toString: String = s"URLContent(url: $url, contentType: $contentType)"

  override def asString: Task[String] = resolved.cachedBytes match {
    case Some(bytes) => Task.pure(new String(bytes, "UTF-8"))
    case None        => Streamer(url, new mutable.StringBuilder).map(_.toString)
  }

  override def asStream: rapid.Stream[Byte] = resolved.cachedBytes match {
    case Some(bytes) => rapid.Stream.emits(ArraySeq.unsafeWrapArray(bytes))
    case None        => rapid.Stream.fromInputStream(Task(url.openStream()))
  }
}
