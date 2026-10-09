package spec

import moduload.Moduload
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.*
import spice.http.client.HttpClient
import spice.http.content.Content
import spice.net.*

import scala.concurrent.duration.*

// The java.net.http backend raises a failed send rather than returning it, so no failure of it reaches the
// retry manager; only the POST case applies.
class JVMRetrySpec extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private lazy val server = new SlowServer(delayMillis = 3000L)

  override protected def beforeAll(): Unit = Moduload.load()

  override protected def afterAll(): Unit = server.close()

  private def client(port: Int, path: String): HttpClient = HttpClient
    .timeout(1.second)
    .url(URL.parse(s"http://127.0.0.1:$port/$path"))

  "a request the server does not answer in time" should {
    "be sent once when it is a POST" in {
      val before = server.methods.size
      val result = client(server.port, "post")
        .post
        .content(Content.string("{}", ContentType.`application/json`))
        .send()
        .attempt
        .sync()
      result.isFailure shouldBe true
      server.methods.drop(before) shouldBe List("POST")
    }
  }
}
