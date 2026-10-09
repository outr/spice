package spice.http.client

object ClientPlatform {
  def defaultSaveDirectory: String = "/"

  /** A browser does not say whether a failed request reached the server, so no failure counts as one before connect. */
  def failedBeforeConnect(throwable: Throwable): Boolean = false
}
