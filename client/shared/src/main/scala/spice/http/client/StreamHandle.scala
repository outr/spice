package spice.http.client

import rapid.{Stream, Task}

/** A streaming HTTP response paired with a handle to abort the in-flight call.
  *
  * `stream` is consumed exactly like the stream returned by `streamLines()`. `cancel` aborts the
  * underlying HTTP call — the consuming side of `stream` then terminates instead of running to
  * natural completion. Calling `cancel` after the stream has already finished is a no-op.
  *
  * @param stream the response body as a stream of lines
  * @param cancel a task that aborts the in-flight call when run
  */
case class StreamHandle[+T](stream: Stream[T], cancel: Task[Unit])
