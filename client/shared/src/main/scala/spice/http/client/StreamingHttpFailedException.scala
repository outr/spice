package spice.http.client

import spice.http.Headers

/**
 * Raised by the [[HttpClient]] streaming path
 * ([[HttpClient.streamLines]] / [[HttpClient.streamLinesHandle]]) when
 * the upstream returns a non-2xx HTTP status. Carries the status code,
 * response headers, and the (truncated) response body so callers can
 * make policy decisions that depend on header values — most notably
 * `retry-after` on a 429 / 503, which the raw message string the
 * exception was previously constructed with had no way to surface.
 *
 * Distinct from a connection-level failure (network down, TLS error)
 * which surfaces as the underlying IOException or implementation-
 * specific exception type. This exception only fires when the upstream
 * returned an HTTP response and that response's status was rejected.
 *
 * Callers can:
 *
 *   - Pattern-match on `status` for category-based handling
 *     (`case e: StreamingHttpFailedException if e.status == 429 => ...`).
 *   - Read `headers.first(HeaderKey.\`Retry-After\`)` (or equivalent)
 *     to compute the upstream's requested cool-off delta.
 *   - Inspect `body` for structured error payloads providers ship
 *     alongside the status (Anthropic's JSON error envelope, etc.).
 *
 * `body` is truncated to a reasonable size at the throw site to keep
 * exception messages bounded; consumers needing the full body should
 * either drain the response through a non-streaming path first or
 * tee the body via a wire interceptor.
 */
final class StreamingHttpFailedException(val status: Int,
                                          val headers: Headers,
                                          val body: String)
  extends RuntimeException(s"HTTP $status: $body")
