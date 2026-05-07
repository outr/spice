package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import scribe.mdc.MDC
import spice.http.content.{Content, StringContent}
import spice.http.server.acme.{AcmeChallengeFilter, AcmeChallengeStore}
import spice.http.server.dsl.FilterResponse
import spice.http.{HttpExchange, HttpRequest, HttpStatus}
import spice.net.*

/** Pins the AcmeChallengeFilter contract:
  *   - Matching path + known token → 200 OK with key authorization as text/plain.
  *   - Matching path + unknown token → 404.
  *   - Non-matching path → Continue (the filter is invisible to other routes). */
class AcmeChallengeFilterSpec extends AnyWordSpec with Matchers {
  private given mdc: MDC = MDC.global

  private def exchange(path: String): HttpExchange =
    HttpExchange(HttpRequest(url = URL.parse(s"http://example.com$path")))

  private def runFilter(path: String, store: AcmeChallengeStore): FilterResponse =
    AcmeChallengeFilter(store).apply(exchange(path)).sync()

  "AcmeChallengeFilter" should {
    "respond 200 with the key authorization when the token is registered" in {
      val store = new AcmeChallengeStore
      store.put("abc123", "abc123.thumbprint")
      val response = runFilter("/.well-known/acme-challenge/abc123", store) match {
        case FilterResponse.Stop(ex) => ex.response
        case other                   => fail(s"Expected Stop, got: $other")
      }
      response.status shouldBe HttpStatus.OK
      response.content match {
        case Some(StringContent(value, contentType, _)) =>
          value shouldBe "abc123.thumbprint"
          contentType shouldBe ContentType.`text/plain`
        case other => fail(s"Expected StringContent, got: $other")
      }
    }

    "respond 404 when the token is not in the store" in {
      val store = new AcmeChallengeStore
      val response = runFilter("/.well-known/acme-challenge/unknown", store) match {
        case FilterResponse.Stop(ex) => ex.response
        case other                   => fail(s"Expected Stop, got: $other")
      }
      response.status shouldBe HttpStatus.NotFound
    }

    "let non-matching paths continue through the filter chain" in {
      val store = new AcmeChallengeStore
      store.put("abc123", "abc123.thumbprint")
      runFilter("/api/health", store) match {
        case FilterResponse.Continue(_) => succeed
        case other                      => fail(s"Expected Continue, got: $other")
      }
    }

    "let near-misses (different prefix) continue through" in {
      val store = new AcmeChallengeStore
      runFilter("/.well-known/something-else/foo", store) match {
        case FilterResponse.Continue(_) => succeed
        case other                      => fail(s"Expected Continue, got: $other")
      }
    }
  }

  "AcmeChallengeStore" should {
    "round-trip token → key authorization" in {
      val store = new AcmeChallengeStore
      store.put("t1", "auth1")
      store.put("t2", "auth2")
      store.get("t1") shouldBe Some("auth1")
      store.get("t2") shouldBe Some("auth2")
      store.get("missing") shouldBe None
    }

    "remove entries on demand" in {
      val store = new AcmeChallengeStore
      store.put("t1", "auth1")
      store.remove("t1")
      store.get("t1") shouldBe None
    }
  }
}
