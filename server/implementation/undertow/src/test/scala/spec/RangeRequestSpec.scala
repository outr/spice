package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import profig.Profig
import rapid.*
import spice.http.*
import spice.http.client.HttpClient
import spice.http.content.Content
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.dsl.*
import spice.http.server.dsl.given
import spice.http.server.handler.SenderHandler
import spice.net.*

import java.nio.file.{Files, Path}

/** A served file answers a byte range (RFC 7233), which every media player wants: a Roku will not play a progressive
  * mp4 without one, Safari will not start it, and seeking or resuming a download needs it everywhere. */
class RangeRequestSpec extends AnyWordSpec with Matchers {
  private val body: String = (0 until 100).map(i => ('a' + (i % 26)).toChar).mkString
  private lazy val file: Path = {
    val p = Files.createTempFile("spice-range", ".txt")
    Files.write(p, body.getBytes("UTF-8"))
    p.toFile.deleteOnExit()
    p
  }

  "a file served by SenderHandler" should {
    object server extends MutableHttpServer
    def serverPort: Int = server.config.listeners().head.port.getOrElse(0)
    lazy val client = HttpClient.url(url"http://localhost".withPort(serverPort))

    "start the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler.matcher(paths.exact("/file.txt")).handle { exchange =>
        SenderHandler(Content.file(file.toFile), replace = true).handle(exchange)(using scribe.mdc.MDC.global)
      }
      server.start().map(_ => server.isRunning should be(true)).sync()
    }
    "send the whole file, and say ranges are understood" in {
      client.path(path"/file.txt").send().flatMap { response =>
        response.status should be(HttpStatus.OK)
        response.headers.first(Headers.Response.`Accept-Ranges`) should be(Some("bytes"))
        response.content.get.asString.map(_ should be(body))
      }.sync()
    }
    "send the range asked for" in {
      client.path(path"/file.txt").header(Headers.Request.`Range`("bytes=10-19")).send().flatMap { response =>
        response.status should be(HttpStatus.PartialContent)
        response.headers.first(Headers.Response.`Content-Range`) should be(Some("bytes 10-19/100"))
        response.headers.first(Headers.`Content-Length`) should be(Some("10"))
        response.content.get.asString.map(_ should be(body.substring(10, 20)))
      }.sync()
    }
    "send from an offset to the end where no end is given" in {
      client.path(path"/file.txt").header(Headers.Request.`Range`("bytes=90-")).send().flatMap { response =>
        response.status should be(HttpStatus.PartialContent)
        response.headers.first(Headers.Response.`Content-Range`) should be(Some("bytes 90-99/100"))
        response.content.get.asString.map(_ should be(body.substring(90)))
      }.sync()
    }
    "send the last bytes for a suffix range" in {
      client.path(path"/file.txt").header(Headers.Request.`Range`("bytes=-5")).send().flatMap { response =>
        response.status should be(HttpStatus.PartialContent)
        response.headers.first(Headers.Response.`Content-Range`) should be(Some("bytes 95-99/100"))
        response.content.get.asString.map(_ should be(body.substring(95)))
      }.sync()
    }
    "cut a range that runs past the end back to the end" in {
      client.path(path"/file.txt").header(Headers.Request.`Range`("bytes=95-1000")).send().flatMap { response =>
        response.status should be(HttpStatus.PartialContent)
        response.headers.first(Headers.Response.`Content-Range`) should be(Some("bytes 95-99/100"))
        response.content.get.asString.map(_ should be(body.substring(95)))
      }.sync()
    }
    "refuse a range that starts past the end" in {
      client.path(path"/file.txt").header(Headers.Request.`Range`("bytes=500-600")).send().map { response =>
        response.status should be(HttpStatus.RequestedRangeNotSatisfiable)
        response.headers.first(Headers.Response.`Content-Range`) should be(Some("bytes */100"))
      }.sync()
    }
    "send the whole file for a range it cannot read" in {
      client.path(path"/file.txt").header(Headers.Request.`Range`("items=1-2")).send().flatMap { response =>
        response.status should be(HttpStatus.OK)
        response.content.get.asString.map(_ should be(body))
      }.sync()
    }
    "stop the server" in {
      server.dispose().map(_ => server.isRunning should be(false)).sync()
    }
  }
}
