package spec

import moduload.Moduload
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.*
import spice.http.client.{HttpClient, RetryManager}
import spice.http.content.Content
import spice.net.*

import scala.concurrent.duration.*

class NettySubSecondTimeoutSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private lazy val server = new SlowServer(delayMillis = 3000L)

  override protected def beforeAll(): Unit = Moduload.load()

  override protected def afterAll(): Unit = server.close()

  private def client(path: String): HttpClient = HttpClient
    .timeout(300.millis)
    .retryManager(RetryManager.none)
    .url(URL.parse(s"http://127.0.0.1:${server.port}/$path"))

  /** The result of `send` and how long it took, in milliseconds. */
  private def timed[T](send: => T): (T, Long) = {
    val start = System.nanoTime()
    val result = send
    (result, (System.nanoTime() - start) / 1000000L)
  }

  "a timeout under a second" should {
    "end a GET the server does not answer in time" in {
      val (result, elapsed) = timed(client("get").get.sendTry().sync())
      withClue(s"after ${elapsed}ms: ")(result.isFailure shouldBe true)
      elapsed should be < 2000L
    }
    "end a POST the server does not answer in time" in {
      val (result, elapsed) = timed(
        client("post").post.content(Content.string("{}", ContentType.`application/json`)).sendTry().sync()
      )
      withClue(s"after ${elapsed}ms: ")(result.isFailure shouldBe true)
      elapsed should be < 2000L
    }
  }
}
