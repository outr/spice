package spice.net

case class URLParseFailure(message: String, failureCode: Int, cause: Option[Throwable] = None)

object URLParseFailure {
  val QuickFail: Int = 1
  val InvalidHost: Int = 2
  val EmailAddress: Int = 3
  val InvalidTopLevelDomain: Int = 4
  val Exception: Int = 5

  /** Reserved code for an opaque URI (`data:`, `blob:`, `mailto:`, …)
    * the parser couldn't model. Opaque URIs now parse into a [[URL]]
    * carrying their scheme-specific part in `URL.opaque`, so the
    * parser no longer emits this for the recognised opaque schemes;
    * the code remains for callers that classify by `failureCode`. */
  val OpaqueScheme: Int = 6
}