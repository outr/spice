package spice.http.client

import spice.ImplementationManager
import spice.http.Headers

object HttpClientImplementationManager extends ImplementationManager[HttpClientImplementation, Unit] {
  Headers.DefaultUserAgent = None
  register(_ => JSHttpClientImplementation)
}
