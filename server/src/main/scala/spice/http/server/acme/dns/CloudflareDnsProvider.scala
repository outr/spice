package spice.http.server.acme.dns

import fabric.*
import fabric.io.JsonParser
import fabric.rw.*
import rapid.Task
import spice.http.HttpMethod
import spice.http.client.HttpClient
import spice.net.URL

import scala.concurrent.duration.*

/** Cloudflare DNS-01 provider using the v4 API (`api.cloudflare.com/client/v4`).
  *
  * Auth uses a scoped API token (the modern path) — generate one at
  * `My Profile → API Tokens → Create Token → Edit zone DNS`, scoped to the specific
  * zones you want ACME to manage. The legacy global API key isn't supported.
  *
  * `propagationWait` is a fixed delay after Cloudflare's API confirms the record was
  * created. Cloudflare's anycast propagation is normally <30s but the DNS-01 challenge
  * is sensitive to lookahead caching at the CA's resolver, so 30s is a safe default.
  * Skip the wait by setting `propagationWait = 0.seconds` if you've already verified
  * propagation out-of-band.
  *
  * Network calls go through Spice's [[HttpClient]] so the provider picks up whatever
  * `HttpClientImplementation` the host application has registered (OkHttp, java.net.http,
  * Netty). Bringing your own implementation is the same as anywhere else in Spice. */
case class CloudflareDnsProvider(apiToken: String,
                                 propagationWait: FiniteDuration = 30.seconds,
                                 ttl: Int = 60,
                                 client: HttpClient = HttpClient) extends DnsProvider {
  import CloudflareDnsProvider.*

  override def publish(recordName: String, value: String): Task[Unit] = {
    val name = recordName.stripSuffix(".")
    val recordContent = trimQuotes(value)
    for {
      zoneId <- lookupZoneId(name)
      payload = obj(
        "type"    -> "TXT".json,
        "name"    -> name.json,
        "content" -> recordContent.json,
        "ttl"     -> ttl.json
      )
      _ <- request(HttpMethod.Post, s"$ApiBase/zones/$zoneId/dns_records", Some(payload))
      _ <- if (propagationWait.toMillis > 0) Task.sleep(propagationWait) else Task.unit
    } yield ()
  }

  override def unpublish(recordName: String, value: String): Task[Unit] = {
    val name = recordName.stripSuffix(".")
    val recordContent = trimQuotes(value)
    lookupZoneId(name).flatMap { zoneId =>
      // List, then DELETE each match. Multiple matches can exist if a previous run leaked.
      request(HttpMethod.Get, s"$ApiBase/zones/$zoneId/dns_records?type=TXT&name=$name", None).flatMap { listJson =>
        val ids = listJson("result").asVector.flatMap { rec =>
          if (rec("content").asString == recordContent) Some(rec("id").asString) else None
        }
        Task.sequence(ids.map(id => request(HttpMethod.Delete, s"$ApiBase/zones/$zoneId/dns_records/$id", None).handleError(_ => Task.unit))).map(_ => ())
      }
    }.handleError(_ => Task.unit) // unpublish runs in `finally`; never throw
  }

  /** Resolve `_acme-challenge.foo.example.com` to the Cloudflare zone id for the longest
    * parent zone that's registered on this account. */
  private def lookupZoneId(recordName: String): Task[String] = {
    val labels = recordName.split('.').toList
    // Walk parents from most-specific to least, but skip the record name itself (acme
    // challenges always sit on a child zone, never on a TLD-level zone).
    val candidates = labels.tails.toList.collect {
      case lst if lst.length >= 2 && lst.length < labels.length => lst.mkString(".")
    }
    def tryNext(remaining: List[String]): Task[String] = remaining match {
      case Nil => Task.error(new RuntimeException(
        s"Cloudflare: no zone found for $recordName (tried: ${candidates.mkString(", ")})"
      ))
      case head :: tail =>
        request(HttpMethod.Get, s"$ApiBase/zones?name=$head", None).flatMap { resp =>
          resp("result").asVector.headOption.map(r => Task.pure(r("id").asString))
            .getOrElse(tryNext(tail))
        }.handleError(_ => tryNext(tail))
    }
    tryNext(candidates)
  }

  private def request(method: HttpMethod, urlString: String, bodyJson: Option[Json]): Task[Json] = {
    val url = URL.parse(urlString)
    val withAuth = client.url(url).method(method).header("Authorization", s"Bearer $apiToken")
    val withBody = bodyJson.fold(withAuth)(j => withAuth.json(j))
    withBody.noFailOnHttpStatus.send().flatMap { response =>
      response.content match {
        case Some(c) => c.asString.flatMap { body =>
          val parsed = JsonParser(body)
          val ok = response.status.isSuccess && parsed("success").asBoolean
          if (ok) Task.pure(parsed)
          else {
            val errs = try parsed("errors").asVector.map(_.toString).mkString("; ") catch { case _: Throwable => "" }
            Task.error(new RuntimeException(
              s"Cloudflare API ${method.value} $urlString failed: status=${response.status} errors=$errs body=$body"
            ))
          }
        }
        case None => Task.error(new RuntimeException(
          s"Cloudflare API ${method.value} $urlString returned no body (status=${response.status})"
        ))
      }
    }
  }
}

object CloudflareDnsProvider {
  private val ApiBase: String = "https://api.cloudflare.com/client/v4"

  /** acme4j's `Dns01Challenge.getDigest()` returns a raw base64url string with no quotes,
    * so this is normally a no-op. Strip leading/trailing double-quotes defensively in
    * case a future caller passes a quoted RR-style string. */
  private def trimQuotes(s: String): String = {
    val stripped = s.trim
    if (stripped.length >= 2 && stripped.startsWith("\"") && stripped.endsWith("\"")) stripped.substring(1, stripped.length - 1)
    else stripped
  }
}
