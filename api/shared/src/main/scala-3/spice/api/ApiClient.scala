package spice.api

import spice.net.URL

object ApiClient {
  inline def derive[T](baseUrl: URL): T = ${ ApiClientMacro.derive[T]('baseUrl) }
}
