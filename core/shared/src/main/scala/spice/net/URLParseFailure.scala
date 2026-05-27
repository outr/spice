package spice.net

case class URLParseFailure(message: String, failureCode: Int, cause: Option[Throwable] = None)

object URLParseFailure {
  val QuickFail: Int = 1
  val InvalidHost: Int = 2
  val EmailAddress: Int = 3
  val InvalidTopLevelDomain: Int = 4
  val Exception: Int = 5

  /** Sigil #296 — the input carries a known opaque URI scheme
    * (`data:`, `blob:`, `mailto:`, etc.). `spice.net.URL` models
    * hierarchical URIs (`scheme://host[:port]/path`); opaque URIs
    * have no host/port/path to populate and the prior best-effort
    * parse rewrote them into garbage (`data:image/png;base64,…`
    * → `https://data:image/png;base64%2C…`). Fail fast with this
    * code so callers know to handle the bytes through a different
    * abstraction (Sigil's `ResponseContent.ImageBytes`, etc.). */
  val OpaqueScheme: Int = 6
}