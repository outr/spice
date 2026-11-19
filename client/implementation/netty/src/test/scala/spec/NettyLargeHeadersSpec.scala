package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import spice.http.client.{HttpClient, RetryManager}
import spice.net.URL

import java.net.ServerSocket
import java.nio.charset.StandardCharsets
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

/** A response whose headers run past Netty's default 8 KiB is read when within the client's limit, and fails at once,
  * not at the read timeout, when over it. */
class NettyLargeHeadersSpec extends AnyWordSpec with Matchers {
  private def serving(headerBytes: Int): ServerSocket = {
    val server = new ServerSocket(0)
    val thread = new Thread(new Runnable {
      override def run(): Unit = Try {
        while (true) {
          val socket = server.accept()
          val in = socket.getInputStream
          val request = new StringBuilder
          while (!request.endsWith("\r\n\r\n")) request.append(in.read().toChar)
          val body = "hello"
          val response = "HTTP/1.1 200 OK\r\n" +
            s"Content-Security-Policy: default-src 'self' ${"x" * headerBytes}\r\n" +
            s"Content-Type: text/plain\r\nContent-Length: ${body.length}\r\n\r\n$body"
          socket.getOutputStream.write(response.getBytes(StandardCharsets.UTF_8))
          socket.getOutputStream.flush()
          // The connection is kept open, as a server keeping it alive does.
          val held = socket
          new Thread(() => { Try(Thread.sleep(60000L)); Try(held.close()); () }).start()
        }
      }
    })
    thread.setDaemon(true)
    thread.start()
    server
  }

  private def get(server: ServerSocket) = HttpClient.timeout(30.seconds).retryManager(RetryManager.none)
    .url(URL.parse(s"http://127.0.0.1:${server.getLocalPort}/page")).get.send().attempt
    .guarantee(Task(Try(server.close())).unit)

  "A response with large headers" should {
    "be read when its headers are within the limit" in {
      get(serving(16 * 1024)).flatMap {
        case Success(response) => response.content.get.asString.map(_ shouldBe "hello")
        case Failure(t) => Task(fail(s"expected the response, got $t"))
      }.sync()
    }
    "fail at once when its headers are over the limit" in {
      val started = System.currentTimeMillis()
      get(serving(200 * 1024)).map {
        case Failure(_) => (System.currentTimeMillis() - started) should be < 10000L
        case Success(response) => fail(s"expected a failure, got status ${response.status.code}")
      }.sync()
    }
    "fail a stream at once when its headers are over the limit" in {
      val server = serving(200 * 1024)
      val started = System.currentTimeMillis()
      HttpClient.timeout(30.seconds).retryManager(RetryManager.none).url(URL.parse(s"http://127.0.0.1:${server.getLocalPort}/page"))
        .get.streamLines().flatMap(_.toList).attempt.guarantee(Task(Try(server.close())).unit).map {
          case Failure(_) => (System.currentTimeMillis() - started) should be < 10000L
          case Success(lines) => fail(s"expected a failure, got $lines")
        }.sync()
    }
  }
}
