package spice.http.server.middleware

import rapid.Task
import scribe.mdc.MDC
import spice.http.{Headers, HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.dsl.{ConnectionFilter, FilterResponse}
import spice.net.IP

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

class RateLimitFilter(
  maxRequests: Int,
  windowMillis: Long,
  keyExtractor: HttpExchange => String = RateLimitFilter.byIP
) extends ConnectionFilter {
  private val buckets = new ConcurrentHashMap[String, RateLimitFilter.Bucket]()

  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = Task {
    val key = keyExtractor(exchange)
    val now = System.currentTimeMillis()
    val bucket = buckets.computeIfAbsent(key, _ => new RateLimitFilter.Bucket(now, new AtomicLong(0)))

    // Reset bucket if window has expired
    if (now - bucket.windowStart > windowMillis) {
      bucket.windowStart = now
      bucket.count.set(0)
    }

    val count = bucket.count.incrementAndGet()
    if (count > maxRequests) {
      val retryAfter = ((bucket.windowStart + windowMillis - now) / 1000) + 1
      stop(exchange.copy(response = exchange.response
        .withStatus(HttpStatus.TooManyRequests)
        .withHeader(Headers.Response.`Retry-After`(retryAfter.toString))
        .withContent(Content.string("Rate limit exceeded", spice.net.ContentType.`text/plain`))
      ))
    } else {
      continue(exchange)
    }
  }
}

object RateLimitFilter {
  private[middleware] class Bucket(@volatile var windowStart: Long, val count: AtomicLong)

  val byIP: HttpExchange => String = exchange => exchange.request.source.toString

  def apply(maxRequests: Int, windowMillis: Long, keyExtractor: HttpExchange => String = byIP): RateLimitFilter =
    new RateLimitFilter(maxRequests, windowMillis, keyExtractor)
}
