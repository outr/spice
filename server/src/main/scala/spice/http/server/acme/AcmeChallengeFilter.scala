package spice.http.server.acme

import rapid.Task
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.StringContent
import spice.http.server.dsl.{ConnectionFilter, FilterResponse}
import spice.net.ContentType

/** Serves HTTP-01 ACME challenges from an [[AcmeChallengeStore]].
  *
  * Matches `/.well-known/acme-challenge/<token>`. When the token is present in the store,
  * responds 200 OK with the key authorization as `text/plain`. When absent, responds 404.
  * Non-matching paths fall through (`FilterResponse.Stop`) so other filters in the chain
  * have their chance — this filter is meant to be installed at the front of the chain on
  * the plain-HTTP listener so the CA's validation request is handled before any auth or
  * routing layer can reject it.
  *
  * Path-prefix detection uses the decoded path string rather than walking `URLPath.parts`
  * because `URLPath` strips empty segments and the leading dot in `.well-known` survives
  * encoding intact. */
case class AcmeChallengeFilter(store: AcmeChallengeStore) extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
    val decoded = exchange.request.url.path.decoded
    if (decoded.startsWith(AcmeChallengeFilter.PathPrefix)) {
      val token = decoded.substring(AcmeChallengeFilter.PathPrefix.length)
      store.get(token) match {
        case Some(keyAuth) =>
          val content = StringContent(keyAuth, ContentType.`text/plain`)
          exchange.modify { response =>
            Task.pure(response.withContent(content).withStatus(HttpStatus.OK))
          }.map(ex => FilterResponse.Stop(ex.finish()))
        case None =>
          exchange.modify { response =>
            Task.pure(response.withStatus(HttpStatus.NotFound))
          }.map(ex => FilterResponse.Stop(ex.finish()))
      }
    } else {
      Task.pure(FilterResponse.Continue(exchange))
    }
  }
}

object AcmeChallengeFilter {
  val PathPrefix: String = "/.well-known/acme-challenge/"
}
