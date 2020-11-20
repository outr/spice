package spice

import spice.net.{IP, IPv4, IPv6}

import scala.reflect.macros.blackbox

class Macros {
  def interpolateIP(context: blackbox.Context)(args: context.Expr[Any]*): context.Expr[IP] = {
    import context.universe._

    context.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) => {
        val parts = rawParts map { case t @ Literal(Constant(const: String)) => (const, t.pos) }

        val b = new StringBuilder
        parts.zipWithIndex.foreach {
          case ((raw, _), index) => {
            if (index > 0) {
              context.abort(context.enclosingPosition, "IP interpolation can only contain string literals. Use IP.apply for runtime parsing.")
            }
            b.append(raw)
          }
        }
        IP(b.toString()) match {
          case ip: IPv4 => context.Expr[IP](q"IPv4(${ip.a}, ${ip.b}, ${ip.c}, ${ip.d})")
          case IPv6(a, b, c, d, e, f, g, h, scope) => context.Expr[IP](q"IPv6($a, $b, $c, $d, $e, $f, $g, $h, $scope)")
        }
      }
      case _ => context.abort(context.enclosingPosition, "Bad usage of IP interpolation.")
    }
  }
}
