package spec

import moduload.Moduload
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.http.*
import spice.http.client.HttpClient
import spice.net.*

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class NettyDaemonThreadsSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private lazy val server = new HeaderCaptureServer

  override protected def beforeAll(): Unit = Moduload.load()

  override protected def afterAll(): Unit = server.close()

  private def liveThreads: Set[Thread] = Thread.getAllStackTraces.keySet.asScala.toSet

  "an HTTP client" should {
    "create only daemon threads, so it never keeps the JVM alive after main returns" in {
      val before = liveThreads
      // A timeout no other spec uses gives this client its own instance and event loop group.
      HttpClient
        .timeout(17.seconds + 321.millis)
        .url(URL.parse(s"http://127.0.0.1:${server.port}/daemon"))
        .method(HttpMethod.Get)
        .noFailOnHttpStatus
        .send()
        .sync()
      server.next()
      val created = liveThreads -- before
      created should not be empty
      created.filterNot(_.isDaemon).map(_.getName) shouldBe empty
    }
  }
}
