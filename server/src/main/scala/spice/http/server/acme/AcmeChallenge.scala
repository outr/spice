package spice.http.server.acme

import spice.http.server.acme.dns.DnsProvider

import scala.concurrent.duration.*

/** Which ACME challenge to use when validating identifiers in an order.
  *
  *   - [[AcmeChallenge.Http01]]: CA visits `http://<domain>/.well-known/acme-challenge/<token>`.
  *     Doesn't work for wildcard (`*.example.com`) identifiers — DNS-01 is the only path.
  *   - [[AcmeChallenge.Dns01]]: CA queries `_acme-challenge.<domain>` TXT. Required for
  *     wildcards; works for non-wildcards too. Needs a [[DnsProvider]] to publish records.
  *
  * Mixing is not supported in a single order — pick the challenge type that satisfies
  * every identifier. If any domain in the order is a wildcard you must use `Dns01`. */
sealed trait AcmeChallenge

object AcmeChallenge {
  case object Http01 extends AcmeChallenge

  /** DNS-01 validation against `provider`. `propagationWait` is an ADDITIONAL delay
    * after `provider.publish` returns — useful when the provider's API confirms record
    * creation but propagation to public resolvers may lag. Set to `Duration.Zero` if
    * the provider already waits internally (e.g. `CloudflareDnsProvider` has its own
    * `propagationWait` field; setting both is redundant but harmless). */
  case class Dns01(provider: DnsProvider,
                   propagationWait: FiniteDuration = Duration.Zero) extends AcmeChallenge
}
