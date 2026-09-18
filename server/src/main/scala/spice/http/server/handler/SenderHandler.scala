package spice.http.server.handler

import rapid.Task
import scribe.mdc.MDC
import spice.http.content.{Content, FileContent}
import spice.http.{Headers, HttpExchange, HttpStatus}

case class SenderHandler(content: Content,
                         length: Option[Long] = None,
                         caching: CachingManager = CachingManager.Default,
                         replace: Boolean = false) extends HttpHandler {
  override def handle(exchange: HttpExchange)(using mdc: MDC): Task[HttpExchange] =
    SenderHandler.handle(exchange, content, length, caching, replace)
}

object SenderHandler {
  def handle(exchange: HttpExchange,
             content: Content,
             length: Option[Long] = None,
             caching: CachingManager = CachingManager.Default,
             replace: Boolean = false): Task[HttpExchange] = {
    if (exchange.response.content.nonEmpty && !replace) {
      throw new RuntimeException(s"Content already set (${exchange.response.content.get}) for HttpResponse in ${exchange.request.url} when attempting to set $content.")
    }
    content match {
      // A file says which of its bytes are wanted (RFC 7233): a media player asks for a range before it will play at
      // all (a Roku refuses a progressive mp4 without one, and so does Safari), a seek asks for the bytes it lands
      // on, and a download interrupted asks to carry on where it stopped.
      case fc: FileContent if fc.whole && length.isEmpty => ranged(exchange, fc, caching)
      case _ => send(exchange, content, length.getOrElse(content.length), caching)
    }
  }

  private def send(exchange: HttpExchange,
                   content: Content,
                   length: Long,
                   caching: CachingManager): Task[HttpExchange] =
    exchange.modify { response =>
      // setHeader (replace) — `withContent` already wrote Content-Length; using `withHeader`
      // here would APPEND a duplicate, producing `Content-Length: N, N` on the wire which
      // clients (per RFC 7230 §3.3.2) treat as malformed and may strip entirely.
      Task(response.withContent(content).setHeader(Headers.`Content-Length`(length)))
    }.flatMap(caching.handle)

  private def ranged(exchange: HttpExchange, content: FileContent, caching: CachingManager): Task[HttpExchange] = {
    val total = content.length
    // every answer says a range may be asked for, so a player knows to ask before it decides it cannot play
    val accepting: Task[HttpExchange] => Task[HttpExchange] =
      _.flatMap(_.modify(r => Task(r.setHeader(Headers.Response.`Accept-Ranges`("bytes")))))
    accepting(asked(exchange, total) match {
      case None => send(exchange, content, total, caching)
      // bytes that are not there: say how many there are, and send none of them (RFC 7233 §4.4). The body is an
      // explicit empty one — an exchange left with no content at all is answered 404 further down the chain.
      case Some((from, to)) if from < 0L || from >= total || to < from =>
        exchange.withContent(Content.empty).flatMap(_.modify { response =>
          Task(response
            .withStatus(HttpStatus.RequestedRangeNotSatisfiable)
            .setHeader(Headers.Response.`Content-Range`(s"bytes */$total"))
            .setHeader(Headers.`Content-Length`(0L)))
        })
      case Some((from, to)) =>
        val window = content.range(from, to - from + 1L)
        exchange.modify { response =>
          Task(response
            .withStatus(HttpStatus.PartialContent)
            .withContent(window)
            .setHeader(Headers.Response.`Content-Range`(s"bytes $from-$to/$total"))
            .setHeader(Headers.`Content-Length`(window.length)))
        }.flatMap(caching.handle)
    })
  }

  /** The first and last byte asked for, where a single readable byte range was asked for: `bytes=10-19`, `bytes=90-`
    * (on to the end) or `bytes=-5` (the last five). Anything else — another unit, several ranges at once, words where
    * numbers belong — is no range this understands, and the whole of it is sent instead. */
  private[spice] def asked(exchange: HttpExchange, total: Long): Option[(Long, Long)] =
    Headers.Request.`Range`.value(exchange.request.headers)
      .map(_.trim)
      .filter(_.toLowerCase.startsWith("bytes="))
      .map(_.substring("bytes=".length).trim)
      .filterNot(_.contains(','))
      .flatMap { spec =>
        spec.split("-", -1).toList match {
          case from :: to :: Nil =>
            (from.trim, to.trim) match {
              case ("", last)  => last.toLongOption.filter(_ > 0L).map(n => (math.max(0L, total - n), total - 1L))
              case (first, "") => first.toLongOption.map(f => (f, total - 1L))
              case (first, last) =>
                for {
                  f <- first.toLongOption
                  l <- last.toLongOption
                } yield (f, math.min(l, total - 1L))
            }
          case _ => None
        }
      }
}
