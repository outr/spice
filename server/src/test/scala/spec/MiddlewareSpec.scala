package spec

import rapid.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpRequest, HttpResponse, HttpStatus, Headers}
import spice.http.content.Content
import spice.http.server.dsl.FilterResponse
import spice.http.server.middleware.{MaxContentLengthFilter, RateLimitFilter, SecurityHeadersFilter}
import spice.net.*

class MiddlewareSpec extends AnyWordSpec with Matchers {
  given mdc: MDC = MDC.instance

  private def exchange(
    headers: Headers = Headers.default,
    responseHeaders: Headers = Headers.empty
  ): HttpExchange = {
    val request = HttpRequest(headers = headers)
    HttpExchange(
      request = request,
      response = HttpResponse(headers = responseHeaders),
      path = URLPath.empty,
      store = new spice.store.MapStore(),
      finished = false
    )
  }

  "SecurityHeadersFilter" when {
    "using default configuration" should {
      val filter = SecurityHeadersFilter.Default

      "add Strict-Transport-Security header" in {
        val ex = exchange()
        val result = filter.apply(ex).sync()
        result shouldBe a[FilterResponse.Continue]
        val response = result.exchange.response
        val hsts = response.headers.first(Headers.Response.`Strict-Transport-Security`)
        hsts shouldBe defined
        hsts.get should include("max-age=31536000")
        hsts.get should include("includeSubDomains")
      }

      "add X-Frame-Options header" in {
        val ex = exchange()
        val result = filter.apply(ex).sync()
        val response = result.exchange.response
        val frameOptions = response.headers.first(Headers.Response.`X-Frame-Options`)
        frameOptions shouldBe Some("DENY")
      }

      "add X-Content-Type-Options header" in {
        val ex = exchange()
        val result = filter.apply(ex).sync()
        val response = result.exchange.response
        val ctOptions = response.headers.first(Headers.Response.`X-Content-Type-Options`)
        ctOptions shouldBe Some("nosniff")
      }

      "always return Continue" in {
        val ex = exchange()
        val result = filter.apply(ex).sync()
        result shouldBe a[FilterResponse.Continue]
      }
    }

    "configured with custom values" should {
      "omit HSTS when hstsMaxAge is None" in {
        val filter = SecurityHeadersFilter(hstsMaxAge = None)
        val ex = exchange()
        val result = filter.apply(ex).sync()
        val response = result.exchange.response
        val hsts = response.headers.first(Headers.Response.`Strict-Transport-Security`)
        hsts shouldBe None
      }

      "omit includeSubDomains when hstsIncludeSubDomains is false" in {
        val filter = SecurityHeadersFilter(hstsIncludeSubDomains = false)
        val ex = exchange()
        val result = filter.apply(ex).sync()
        val response = result.exchange.response
        val hsts = response.headers.first(Headers.Response.`Strict-Transport-Security`)
        hsts shouldBe defined
        hsts.get should not include "includeSubDomains"
      }

      "use custom frame options value" in {
        val filter = SecurityHeadersFilter(frameOptions = Some("SAMEORIGIN"))
        val ex = exchange()
        val result = filter.apply(ex).sync()
        val response = result.exchange.response
        val frameOptions = response.headers.first(Headers.Response.`X-Frame-Options`)
        frameOptions shouldBe Some("SAMEORIGIN")
      }

      "omit X-Content-Type-Options when contentTypeOptions is false" in {
        val filter = SecurityHeadersFilter(contentTypeOptions = false)
        val ex = exchange()
        val result = filter.apply(ex).sync()
        val response = result.exchange.response
        val ctOptions = response.headers.first(Headers.Response.`X-Content-Type-Options`)
        ctOptions shouldBe None
      }

      "add Content-Security-Policy when configured" in {
        val csp = "default-src 'self'"
        val filter = SecurityHeadersFilter(contentSecurityPolicy = Some(csp))
        val ex = exchange()
        val result = filter.apply(ex).sync()
        val response = result.exchange.response
        val cspHeader = response.headers.first(Headers.Response.`Content-Security-Policy`)
        cspHeader shouldBe Some(csp)
      }
    }
  }

  "MaxContentLengthFilter" when {
    "request is within size limit" should {
      "return Continue" in {
        val filter = MaxContentLengthFilter(1024L)
        val headers = Headers.default.withHeader(Headers.`Content-Length`(512L))
        val ex = exchange(headers = headers)
        val result = filter.apply(ex).sync()
        result shouldBe a[FilterResponse.Continue]
      }

      "return Continue when no Content-Length header is present" in {
        val filter = MaxContentLengthFilter(1024L)
        val ex = exchange()
        val result = filter.apply(ex).sync()
        result shouldBe a[FilterResponse.Continue]
      }
    }

    "request exceeds size limit" should {
      "return Stop with RequestEntityTooLarge status" in {
        val filter = MaxContentLengthFilter(1024L)
        val headers = Headers.default.withHeader(Headers.`Content-Length`(2048L))
        val ex = exchange(headers = headers)
        val result = filter.apply(ex).sync()
        result shouldBe a[FilterResponse.Stop]
        result.exchange.response.status should be(HttpStatus.RequestEntityTooLarge)
      }

      "include an error message in the response body" in {
        val filter = MaxContentLengthFilter(1024L)
        val headers = Headers.default.withHeader(Headers.`Content-Length`(2048L))
        val ex = exchange(headers = headers)
        val result = filter.apply(ex).sync()
        val content = result.exchange.response.content
        content shouldBe defined
        val str = content.get.asString.sync()
        str should include("1024")
        str should include("too large")
      }

      "reject at exact boundary (maxBytes + 1)" in {
        val filter = MaxContentLengthFilter(100L)
        val headers = Headers.default.withHeader(Headers.`Content-Length`(101L))
        val ex = exchange(headers = headers)
        val result = filter.apply(ex).sync()
        result shouldBe a[FilterResponse.Stop]
      }

      "allow at exact boundary (maxBytes)" in {
        val filter = MaxContentLengthFilter(100L)
        val headers = Headers.default.withHeader(Headers.`Content-Length`(100L))
        val ex = exchange(headers = headers)
        val result = filter.apply(ex).sync()
        result shouldBe a[FilterResponse.Continue]
      }
    }
  }

  "RateLimitFilter" when {
    "requests are within the rate limit" should {
      "return Continue for requests under the limit" in {
        val filter = RateLimitFilter(maxRequests = 5, windowMillis = 60000L)
        val ex = exchange()
        val result = filter.apply(ex).sync()
        result shouldBe a[FilterResponse.Continue]
      }

      "allow exactly maxRequests requests" in {
        val filter = RateLimitFilter(maxRequests = 3, windowMillis = 60000L)
        for (_ <- 1 to 3) {
          val ex = exchange()
          val result = filter.apply(ex).sync()
          result shouldBe a[FilterResponse.Continue]
        }
      }
    }

    "requests exceed the rate limit" should {
      "return Stop with TooManyRequests status after exceeding limit" in {
        val filter = RateLimitFilter(maxRequests = 2, windowMillis = 60000L)
        // Use up the allowed requests
        filter.apply(exchange()).sync()
        filter.apply(exchange()).sync()

        // Third request should be rejected
        val result = filter.apply(exchange()).sync()
        result shouldBe a[FilterResponse.Stop]
        result.exchange.response.status should be(HttpStatus.TooManyRequests)
      }

      "include Retry-After header in the rejection response" in {
        val filter = RateLimitFilter(maxRequests = 1, windowMillis = 60000L)
        filter.apply(exchange()).sync()

        val result = filter.apply(exchange()).sync()
        result shouldBe a[FilterResponse.Stop]
        val retryAfter = result.exchange.response.headers.first(Headers.Response.`Retry-After`)
        retryAfter shouldBe defined
      }

      "include error message in the response body" in {
        val filter = RateLimitFilter(maxRequests = 1, windowMillis = 60000L)
        filter.apply(exchange()).sync()

        val result = filter.apply(exchange()).sync()
        val content = result.exchange.response.content
        content shouldBe defined
        val str = content.get.asString.sync()
        str should include("Rate limit exceeded")
      }
    }

    "using a custom key extractor" should {
      "rate limit independently per key" in {
        var requestKey = "user1"
        val filter = RateLimitFilter(
          maxRequests = 1,
          windowMillis = 60000L,
          keyExtractor = _ => requestKey
        )

        // First request from user1
        val result1 = filter.apply(exchange()).sync()
        result1 shouldBe a[FilterResponse.Continue]

        // Second request from user1 should be rejected
        val result2 = filter.apply(exchange()).sync()
        result2 shouldBe a[FilterResponse.Stop]

        // First request from user2 should be allowed
        requestKey = "user2"
        val result3 = filter.apply(exchange()).sync()
        result3 shouldBe a[FilterResponse.Continue]
      }
    }
  }
}
