package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.client.HttpClient
import spice.net.URL

import java.net.ServerSocket
import scala.concurrent.duration.*
import scala.util.Try

class NettyStreamingLivenessSpec extends AnyWordSpec with Matchers {

  /** A server that writes `first`, pauses, writes `second`, then closes. */
  private def pausingStreamServer(first: String, pauseMs: Long, second: String): Int = {
    val server = new ServerSocket(0)
    val port = server.getLocalPort
    val thread = new Thread(new Runnable {
      override def run(): Unit = {
        try {
          val socket = server.accept()
          socket.getInputStream.read(new Array[Byte](4096))
          val out = socket.getOutputStream
          out.write("HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n".getBytes)
          out.write(first.getBytes); out.flush()
          if (pauseMs > 0) Thread.sleep(pauseMs)
          if (second.nonEmpty) { out.write(second.getBytes); out.flush() }
          Try(socket.close())
        } catch { case _: Throwable => () }
        finally Try(server.close())
      }
    })
    thread.setDaemon(true)
    thread.start()
    port
  }

  private def lines(client: HttpClient, port: Int): List[String] = {
    val stream = client.url(URL.parse(s"http://127.0.0.1:$port/stream")).get.streamLines().sync()
    stream.toList.sync()
  }

  "streamingTimeout" should {
    "keep a streaming response alive across a pause longer than the base read-idle" in {
      val port = pausingStreamServer("data: a\n", pauseMs = 2500L, "data: b\n")
      val result = lines(
        HttpClient.timeout(1.second).streamingTimeout(10.seconds),
        port
      )
      result.filter(_.startsWith("data: ")) should contain allOf ("data: a", "data: b")
    }
  }

  "a streaming response" should {
    "terminate cleanly and promptly when the socket closes mid-stream" in {
      val port = pausingStreamServer("data: a\n", pauseMs = 0L, "")
      val started = System.currentTimeMillis()
      val result = lines(HttpClient.timeout(30.seconds), port)
      val elapsed = System.currentTimeMillis() - started
      result.filter(_.startsWith("data: ")) shouldBe List("data: a")
      elapsed should be < 5000L
    }
  }
}
