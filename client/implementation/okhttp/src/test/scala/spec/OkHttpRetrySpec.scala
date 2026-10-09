package spec

import moduload.Moduload
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import spice.http.*
import spice.http.client.HttpClient
import spice.http.client.intercept.Interceptor
import spice.http.content.Content
import spice.net.*

import java.net.ServerSocket
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.util.Try

class OkHttpRetrySpec extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private lazy val server = new SlowServer(delayMillis = 3000L)

  override protected def beforeAll(): Unit = Moduload.load()

  override protected def afterAll(): Unit = server.close()

  private class CountingInterceptor extends Interceptor {
    val attempts = new AtomicInteger(0)

    override def before(request: HttpRequest): Task[HttpRequest] = Task {
      attempts.incrementAndGet()
      request
    }

    override def after(request: HttpRequest, result: Try[HttpResponse]): Task[Try[HttpResponse]] = Task.pure(result)
  }

  private def client(port: Int, path: String): HttpClient = HttpClient
    .timeout(1.second)
    .url(URL.parse(s"http://127.0.0.1:$port/$path"))

  "a request the server does not answer in time" should {
    "be sent once when it is a POST" in {
      val before = server.methods.size
      val result = client(server.port, "post")
        .post
        .content(Content.string("{}", ContentType.`application/json`))
        .sendTry()
        .sync()
      result.isFailure shouldBe true
      server.methods.drop(before) shouldBe List("POST")
    }
    "be sent again when it is a GET" in {
      val before = server.methods.size
      val result = client(server.port, "get").get.sendTry().sync()
      result.isFailure shouldBe true
      server.methods.drop(before) shouldBe List("GET", "GET", "GET")
    }
  }

  "a POST that cannot connect" should {
    "be tried again" in {
      val closed = new ServerSocket(0)
      val port = closed.getLocalPort
      closed.close()
      val counter = new CountingInterceptor
      val result = client(port, "post")
        .interceptor(counter)
        .post
        .content(Content.string("{}", ContentType.`application/json`))
        .sendTry()
        .sync()
      result.isFailure shouldBe true
      counter.attempts.get() shouldBe 3
    }
  }
}
