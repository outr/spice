package spice.http.client

import rapid.{Stream, Task}
import spice.http.Headers

/** A streaming HTTP response paired with a handle to abort the in-flight call.
  *
  * `stream` is consumed exactly like the stream returned by `streamLines()`. `cancel` aborts the
  * underlying HTTP call — the consuming side of `stream` then terminates instead of running to
  * natural completion. Calling `cancel` after the stream has already finished is a no-op.
  *
  * `responseHeaders` is a `Task[Headers]` that completes when the upstream's response headers
  * arrive — surfaces metadata like `x-ratelimit-*` / `cache-control` / `content-type` for
  * callers that need to adapt subsequent calls (e.g. populating
  * `Model.inputTokensPerMinute` from Anthropic's `anthropic-ratelimit-input-tokens-limit`).
  * Backends with synchronous header arrival (OkHttp's `onResponse`, java.net.http's response
  * object) pass `Task.pure(headers)` directly; backends with async arrival (Netty) wire a
  * completable that fires when the headers land. Callers that don't need response metadata
  * simply don't await this Task.
  *
  * Non-2xx responses raise [[StreamingHttpFailedException]] before the StreamHandle is
  * produced, so consumers of this Task can rely on the fact that response headers reaching
  * them correspond to a successful (2xx) status — error-path metadata lives on the
  * exception's `headers` field instead.
  *
  * @param stream the response body as a stream of lines
  * @param cancel a task that aborts the in-flight call when run
  * @param responseHeaders a Task that completes with the upstream's response headers
  */
case class StreamHandle[+T](stream: Stream[T],
                             cancel: Task[Unit],
                             responseHeaders: Task[Headers] = Task.pure(Headers.empty))
