package spice

import scala.language.experimental.macros

package object net {
  implicit class URLInterpolator(val sc: StringContext) extends AnyVal {
    def url(args: Any*): URL = macro NetMacros.interpolateURL
  }

  implicit class IPInterpolator(val sc: StringContext) extends AnyVal {
    def ip(args: Any*): IP = macro NetMacros.interpolateIP
  }

  implicit class PathInterpolation(val sc: StringContext) extends AnyVal {
    def path(args: Any*): Path = macro NetMacros.interpolatePath
  }
}