package spice.http.server.acme

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** In-memory map of `token → keyAuthorization` used during ACME HTTP-01 validation.
  *
  * The CA visits `http://<domain>/.well-known/acme-challenge/<token>` and expects a
  * plain-text body matching the JWK-derived key authorization for that token. Tokens are
  * registered when the order is placed and removed when validation completes (or fails),
  * so the map is normally empty and only briefly non-empty during cert issuance.
  *
  * Concurrent because the validating filter runs on a server thread while the cert flow
  * runs on a Rapid fiber. */
final class AcmeChallengeStore {
  private val map: ConcurrentHashMap[String, String] = new ConcurrentHashMap[String, String]()

  def put(token: String, keyAuthorization: String): Unit = map.put(token, keyAuthorization)

  def get(token: String): Option[String] = Option(map.get(token))

  def remove(token: String): Unit = map.remove(token)

  def size: Int = map.size

  def snapshot: Map[String, String] = map.asScala.toMap
}
