package spec

import rapid.*
import fabric.rw.*
import moduload.Moduload
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.*
import spice.http.client.intercept.Interceptor
import spice.http.client.{HttpClient, OkHttpClientImplementation}
import spice.http.content.*
import spice.net.*

import scala.concurrent.duration.*

class OkHttpClientSpec extends AnyWordSpec with Matchers {
  "OkHttpClient" should {
    "load Moduload" in {
      Moduload.load()
    }
    "be the default implementation" in {
      HttpClient.implementation should be(OkHttpClientImplementation)
    }
    "GET the user-agent" in {
      HttpClient.url(url"https://httpbin.org/user-agent").get.send().map { response =>
        response.status should be(HttpStatus.OK)
        val content = response.content.get.asInstanceOf[StringContent]
        content.value.contains("user-agent") should be(true)
      }
    }
    "call a URL multiple times with a rate limiter" in {
      var calls = 0
      val limiter = Interceptor.rateLimited(1.seconds)

      def callMultiple(counter: Int): Task[Unit] = {
        HttpClient.interceptor(limiter).url(url"https://httpbin.org/user-agent").get.send().flatMap { response =>
          response.status should be(HttpStatus.OK)
          calls += 1
          if (counter > 0) {
            callMultiple(counter - 1)
          } else {
            Task.unit
          }
        }
      }

      val start = System.currentTimeMillis()
      callMultiple(5).map { _ =>
        calls should be(6)
        val elapsed = System.currentTimeMillis() - start
        elapsed <= 10000L should be(true)
      }
    }
    "call a URL and get a case class back" in {
      HttpClient.url(url"https://jsonplaceholder.typicode.com/todos/1").get.callTry[Placeholder].map(_.get).map { p =>
        p.userId should be(1)
        p.id should be(1)
        p.title should be("delectus aut autem")
        p.completed should be(false)
      }
    }
    "restful call to a URL" in {
      HttpClient
        .url(url"https://jsonplaceholder.typicode.com/posts")
        .restfulTry[Placeholder, Placeholder](Placeholder(123, 456, "Test YouI", completed = false)).map(_.get).map { p =>
        p.userId should be(123)
        p.id should be(101)
        p.title should be("Test YouI")
        p.completed should be(false)
      }
    }
    "stream lines from a URL" in {
      HttpClient.url(url"https://httpbin.org/stream/5").get.streamLines().flatMap { stream =>
        stream.toList.map { lines =>
          lines.count(_.nonEmpty) should be(5)
        }
      }
    }
    "cancel an in-flight streaming request" in {
      // A local chunked-transfer server emits one line every 200ms for 50 lines (~10s total).
      // We consume on a fiber, cancel partway through, and assert the stream terminates promptly
      // and the server observes the connection drop — proving Call.cancel() aborted the request.
      val totalLines = 50
      val server = new java.net.ServerSocket(0)
      val port = server.getLocalPort
      @volatile var writeFailedAt: Int = -1
      val serverThread = new Thread(new Runnable {
        override def run(): Unit = {
          try {
            val socket = server.accept()
            val out = socket.getOutputStream
            out.write("HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nTransfer-Encoding: chunked\r\n\r\n".getBytes("UTF-8"))
            out.flush()
            var i = 0
            var failed = false
            while (i < totalLines && !failed) {
              val line = s"data: line $i\n"
              val chunk = s"${Integer.toHexString(line.length)}\r\n$line\r\n"
              try {
                out.write(chunk.getBytes("UTF-8"))
                out.flush()
                Thread.sleep(200L)
              } catch {
                case _: Throwable =>
                  writeFailedAt = i
                  failed = true
              }
              i += 1
            }
            scala.util.Try(socket.close())
          } catch {
            case _: Throwable => // server closed
          }
        }
      })
      serverThread.setDaemon(true)
      serverThread.start()

      val test = for {
        handle <- HttpClient.url(URL.parse(s"http://127.0.0.1:$port/stream")).get.streamLinesHandle()
        start = System.currentTimeMillis()
        consumed <- handle.stream.count.start
        _ <- Task.sleep(1.second)
        _ <- handle.cancel
        lineCount <- consumed.join
      } yield {
        val elapsed = System.currentTimeMillis() - start
        // Natural completion takes ~10s; cancellation must terminate the stream far sooner.
        elapsed should be < 6000L
        // The consumer saw only a fraction of the 50 lines before cancellation.
        lineCount should be < totalLines
      }
      try {
        test.sync()
        // Give the server thread a moment to observe the dropped connection.
        Thread.sleep(500L)
        writeFailedAt should be >= 0
        writeFailedAt should be < totalLines
      } finally {
        scala.util.Try(server.close())
      }
    }
    "call a URL and get an image back" in {
      val url = url"https://s.yimg.com/ny/api/res/1.2/8Qe5c2B.moDrzo4jn7T5VQ--~A/YXBwaWQ9aGlnaGxhbmRlcjt3PTU2MzI7aD0zNzU1O3NtPTE7aWw9cGxhbmU-/https://media-mbst-pub-ue1.s3.amazonaws.com/creatr-images/2020-04/81f62d40-7ff9-11ea-bfdd-25ac22907561.cf.jpg"
      HttpClient.url(url).send().map { response =>
        response.status should be(HttpStatus.OK)
        response.content.map(_.contentType) should be(Some(ContentType.`image/jpeg`))
      }
    }
  }

  case class Placeholder(userId: Int, id: Int, title: String, completed: Boolean)

  object Placeholder {
    given rw: RW[Placeholder] = RW.gen
  }
}
