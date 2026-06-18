package spec

import moduload.Moduload
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.*
import spice.http.client.{ConnectionPool, HttpClient}
import spice.net.*

import java.net.ServerSocket
import scala.concurrent.duration.*
import scala.util.{Try, Using}

/**
 * Regression for the streaming channel-pool leak: a streaming response
 * reconfigures its channel with streaming idle handlers and must not be
 * re-pooled, so it's closed on completion — but `FixedChannelPool` only
 * decrements its acquired-channel count on `release`, so closing WITHOUT
 * releasing leaks the pool slot. Over a session of streaming calls the
 * per-host pool exhausts and every further acquire fails with
 * "Acquire operation took longer then configured maximum time".
 *
 * This shrinks the pool to 2 and makes 5 sequential streaming calls: with
 * the leak the 3rd acquire times out; with the fix every terminal path
 * releases the channel, so all 5 succeed.
 */
class NettyStreamingPoolReleaseSpec extends AnyWordSpec with Matchers {
  "streaming pool release" should {
    "load Moduload" in {
      Moduload.load()
    }

    "release the channel on every streaming call so a small pool isn't exhausted" in {
      val server = new ServerSocket(0)
      val port = server.getLocalPort
      val serverThread = new Thread(() => {
        try {
          while (!server.isClosed) {
            val socket = server.accept()
            Try {
              val in = socket.getInputStream
              // Drain the request headers (until CRLFCRLF) so the client's
              // write completes cleanly before we respond.
              val buf = new Array[Byte](4096)
              var seen = new StringBuilder
              var done = false
              while (!done && in.available() >= 0) {
                val n = in.read(buf)
                if (n <= 0) done = true
                else {
                  seen.append(new String(buf, 0, n, "UTF-8"))
                  if (seen.indexOf("\r\n\r\n") >= 0) done = true
                }
              }
              val out = socket.getOutputStream
              val resp =
                "HTTP/1.1 200 OK\r\n" +
                  "Content-Type: text/plain\r\n" +
                  "Connection: close\r\n" +
                  "Transfer-Encoding: chunked\r\n\r\n" +
                  "6\r\nhello\n\r\n" +
                  "0\r\n\r\n"
              out.write(resp.getBytes("UTF-8"))
              out.flush()
              Thread.sleep(50)
            }
            Try(socket.close())
          }
        } catch { case _: Throwable => () }
      })
      serverThread.setDaemon(true)
      serverThread.start()

      val prevMax = ConnectionPool.maxIdleConnections
      // A unique timeout yields a fresh client instance (instances are cached
      // by a key that includes timeout) whose pool is built reading this size.
      // A short timeout doubles as the acquire timeout — pre-fix the exhausted
      // 3rd acquire fails fast instead of hanging.
      ConnectionPool.maxIdleConnections = 2
      try {
        val client = HttpClient
          .timeout(2753.millis)
          .streamingTimeout(10.seconds)

        def callOnce(): Try[List[String]] = Try {
          client
            .url(URL.parse(s"http://127.0.0.1:$port/stream"))
            .get
            .streamLines()
            .sync()
            .toList
            .sync()
        }

        val results = (1 to 5).map(_ => callOnce()).toList
        Try(server.close())

        val failures = results.zipWithIndex.collect { case (scala.util.Failure(t), i) => s"call ${i + 1}: ${t.getClass.getSimpleName}: ${t.getMessage}" }
        withClue(s"streaming calls that failed (pool exhausted via leaked channels): ${failures.mkString("; ")}\n") {
          failures shouldBe empty
        }
        results.foreach(r => r.toOption.getOrElse(Nil) should contain("hello"))
      } finally {
        ConnectionPool.maxIdleConnections = prevMax
        Try(server.close())
      }
    }
  }
}
