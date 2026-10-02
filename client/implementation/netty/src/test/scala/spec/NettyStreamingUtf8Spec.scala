package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.client.{HttpClient, StreamingHttpFailedException}
import spice.net.URL

import java.net.ServerSocket
import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*
import scala.util.Try

class NettyStreamingUtf8Spec extends AnyWordSpec with Matchers {
  private val line = "data: one — two 🐰 three"
  private val bytes = s"$line\n".getBytes(StandardCharsets.UTF_8)
  private val dash = bytes.indexOf(0xe2.toByte)
  private val rabbit = bytes.indexOf(0xf0.toByte)

  /** A server answering with `status` whose body is written in `parts`, each flushed and followed by a pause so it
    * arrives as its own read. */
  private def splitBodyServer(status: String, parts: List[Array[Byte]]): Int = {
    val server = new ServerSocket(0)
    val port = server.getLocalPort
    val thread = new Thread(() => {
      try {
        val socket = server.accept()
        socket.getInputStream.read(new Array[Byte](4096))
        val out = socket.getOutputStream
        out.write(s"HTTP/1.1 $status\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n".getBytes)
        out.flush()
        parts.foreach { part =>
          out.write(part)
          out.flush()
          Thread.sleep(200L)
        }
        socket.close()
      } catch { case _: Throwable => () }
      finally Try(server.close())
    })
    thread.setDaemon(true)
    thread.start()
    port
  }

  private def split: List[Array[Byte]] = List(
    bytes.slice(0, dash + 1),
    bytes.slice(dash + 1, rabbit + 2),
    bytes.slice(rabbit + 2, bytes.length)
  )

  "a streamed line" should {
    "decode a multi-byte character split across reads" in {
      val port = splitBodyServer("200 OK", split)
      val stream = HttpClient.timeout(10.seconds).url(URL.parse(s"http://127.0.0.1:$port/stream")).get.streamLines().sync()
      stream.toList.sync() shouldBe List(line)
    }
  }

  "a streamed error body" should {
    "decode a multi-byte character split across reads" in {
      val port = splitBodyServer("500 Internal Server Error", split)
      val stream = HttpClient.timeout(10.seconds).url(URL.parse(s"http://127.0.0.1:$port/stream")).get.streamLines().sync()
      val failure = Try(stream.toList.sync()).failed.get
      failure shouldBe a[StreamingHttpFailedException]
      failure.asInstanceOf[StreamingHttpFailedException].body shouldBe s"$line\n"
    }
  }
}
