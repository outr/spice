package spice.net

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class URLPathSegmentsSpec extends AnyWordSpec with Matchers {
  // The look-behind split URLPath.parse used before; on the JVM it still runs, so it is the reference.
  private def regexSplit(path: String): List[String] = path.split("((?<=/)|(?=/))").toList

  private val paths = List(
    "", "/", "//", "///", "a", "/a", "a/", "/a/", "//a", "a//", "/a//b/", "a/b/c", "/a/b/c/",
    "/api/v1/users/42", "/api//v1///users/", "/./a/../b", "../a", "./", "/:id/name", "/{id}/name",
    "/caf%C3%A9/%2F/x", "/a%20b/c+d/", "%2F", "/path with spaces/", "/été/中文/",
    "/emoji/😀/", "/a?b=c/d", "/a#frag/x", "no-slash-at-all"
  )

  "URLPath.segments" should {
    "split as the look-behind regex did on fixed paths" in {
      paths.foreach { p =>
        withClue(s"path '$p': ") {
          URLPath.segments(p) shouldBe regexSplit(p)
        }
      }
    }
    "split as the look-behind regex did on random paths" in {
      val random = new Random(38L)
      val alphabet = "/ab%2F.:{}-_~ é"
      (0 until 5000).foreach { _ =>
        val p = List.fill(random.nextInt(12))(alphabet.charAt(random.nextInt(alphabet.length))).mkString
        withClue(s"path '$p': ") {
          URLPath.segments(p) shouldBe regexSplit(p)
        }
      }
    }
    "keep URLPath.parse's results" in {
      URLPath.parse("/a//b/").encoded shouldBe "/a//b/"
      URLPath.parse("/api/:id/x").arguments shouldBe List("id")
      URLPath.parse("").encoded shouldBe ""
      URL.parse("https://example.com/caf%C3%A9/a%20b/?q=1").path.decoded shouldBe "/café/a b/"
    }
  }
}
