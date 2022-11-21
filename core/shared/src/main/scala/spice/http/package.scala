package spice

import spice.net._

import scala.language.implicitConversions

package object http {
  object combined {
    def all(matchers: URLMatcher*): URLMatcher = (url: URL) => matchers.forall(_.matches(url))

    def any(matchers: URLMatcher*): URLMatcher = (url: URL) => matchers.exists(_.matches(url))
  }

  object urls {
    def exact(urlString: String): URLMatcher = (url: URL) => url.decoded.toString == urlString
  }

  object hosts {
    def exact(host: String): URLMatcher = (url: URL) => url.host.equalsIgnoreCase(host)
    def matches(regex: String): URLMatcher = (url: URL) => url.host.matches(regex)
  }

  object paths {
    def exact(path: String): URLMatcher = (url: URL) => url.path.decoded == path
    def exact(path: Path): URLMatcher = (url: URL) => url.path == path
    def matches(regex: String): URLMatcher = (url: URL) => url.path.decoded.matches(regex)
    def startsWith(prefix: String): URLMatcher = (url: URL) => url.path.decoded.startsWith(prefix)
    def endsWith(prefix: String): URLMatcher = (url: URL) => url.path.decoded.endsWith(prefix)
  }

  object all extends URLMatcher {
    override def matches(url: URL): Boolean = true
  }
}