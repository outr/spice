package spice.http.server.acme.dns

import rapid.Task

/** A DNS-01 challenge requires publishing a TXT record at `_acme-challenge.<domain>`
  * with a value derived from the challenge key. A `DnsProvider` knows how to publish
  * (and clean up) such records against a specific DNS host.
  *
  * `recordName` is the FQDN to set (e.g. `_acme-challenge.example.com`). `value` is the
  * exact string the CA expects to read from the TXT record (already base64url-encoded by
  * acme4j — the provider just needs to write it verbatim).
  *
  * Implementations should:
  *   - `publish` returns only after the record is observably visible from public
  *     resolvers. Provider APIs that confirm internal propagation count; if not, wrap
  *     this method (or chain a `WaitForDns` after it) before handing it to AcmeManager.
  *   - `unpublish` should not throw if the record was already cleaned up — it runs in
  *     a `finally` and a leaked record is annoying but not catastrophic.
  *   - Both methods are idempotent. The ACME flow may retry. */
trait DnsProvider {
  def publish(recordName: String, value: String): Task[Unit]
  def unpublish(recordName: String, value: String): Task[Unit]
}

object DnsProvider {
  /** Decompose `_acme-challenge.foo.example.com` into the apex zone (`example.com`) and
    * the subdomain part (`_acme-challenge.foo`). Provider APIs that key on the apex zone
    * use the first; the second is the record's name within the zone.
    *
    * The split is naïve — last two labels = apex. This is correct for typical
    * `example.com`/`foo.example.com` setups but wrong for multi-label TLDs like
    * `example.co.uk`. Providers that need a real public-suffix list should override
    * this in their own helper rather than retrofitting it here. */
  def splitApex(recordName: String): (String, String) = {
    val labels = recordName.stripSuffix(".").split('.').toList
    if (labels.length <= 2) ("", recordName.stripSuffix("."))
    else {
      val apex = labels.takeRight(2).mkString(".")
      val name = labels.dropRight(2).mkString(".")
      (apex, name)
    }
  }
}
