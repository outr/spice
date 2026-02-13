package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import spice.http.client.HttpClient
import spice.net.URL

import java.net.ServerSocket
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

class NettyHttpClientReliabilitySpec extends AnyWordSpec with Matchers {
  "NettyHttpClient pooled lifecycle" should {
    "complete with failure on read timeout (no hang)" in {
      val server = new ServerSocket(0)
      val port = server.getLocalPort
      val thread = new Thread(new Runnable {
        override def run(): Unit = {
        val socket = server.accept()
        // Intentionally never write a response to force read timeout.
        Thread.sleep(5000L)
        Try(socket.close())
        }
      })
      thread.setDaemon(true)
      thread.start()

      val started = System.currentTimeMillis()
      HttpClient
        .timeout(1.second)
        .url(URL.parse(s"http://127.0.0.1:$port/timeout"))
        .get
        .send()
        .attempt
        .guarantee(Task(Try(server.close())).unit)
        .map {
          case Failure(_) =>
            val elapsed = System.currentTimeMillis() - started
            elapsed should be < 10000L
            succeed
          case Success(resp) =>
            fail(s"Expected failure from stalled response, but got status=${resp.status.code}")
        }
    }

    "complete with failure when server closes early (no hang)" in {
      val server = new ServerSocket(0)
      val port = server.getLocalPort
      val thread = new Thread(new Runnable {
        override def run(): Unit = {
        val socket = server.accept()
        // Close immediately before any response is written.
        Try(socket.close())
        }
      })
      thread.setDaemon(true)
      thread.start()

      val started = System.currentTimeMillis()
      HttpClient
        .timeout(2.seconds)
        .url(URL.parse(s"http://127.0.0.1:$port/close"))
        .get
        .send()
        .attempt
        .guarantee(Task(Try(server.close())).unit)
        .map {
          case Failure(_) =>
            val elapsed = System.currentTimeMillis() - started
            elapsed should be < 10000L
            succeed
          case Success(resp) =>
            fail(s"Expected failure from early close, but got status=${resp.status.code}")
        }
    }

    "complete all requests under concurrent failure load" in {
      val server = new ServerSocket(0)
      val port = server.getLocalPort
      val acceptThread = new Thread(new Runnable {
        override def run(): Unit = {
          var continue = true
          while (continue) {
            try {
              val socket = server.accept()
              Try(socket.close())
            } catch {
              case _: Throwable =>
                continue = false
            }
          }
        }
      })
      acceptThread.setDaemon(true)
      acceptThread.start()

      val total = 50
      val requests = rapid.Stream
        .emits((0 until total).toVector)
        .par(16) { _ =>
          HttpClient
            .timeout(2.seconds)
            .url(URL.parse(s"http://127.0.0.1:$port/fail-fast"))
            .get
            .send()
            .attempt
        }
        .toList

      requests
        .guarantee(Task(Try(server.close())).unit)
        .map { results =>
          results.length should be(total)
          results.count(_.isFailure) should be(total)
        }
    }
  }
}

