package spice.net

import scala.reflect.macros.blackbox

object NetMacros {
  def interpolateIP(context: blackbox.Context)(args: context.Expr[Any]*): context.Expr[IP] = {
    import context.universe._

    context.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) => {
        val parts = rawParts map { case t@Literal(Constant(const: String)) => (const, t.pos) }

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

  def interpolatePath(c: blackbox.Context)(args: c.Expr[Any]*): c.Expr[Path] = {
    import c.universe._

    implicit val liftablePathPart: Liftable[PathPart] = new Liftable[PathPart] {
      override def apply(value: PathPart): c.universe.Tree = {
        q"""io.youi.net.PathPart(${value.value}).getOrElse(throw new RuntimeException("Invalid PathPart value"))"""
      }
    }

    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) => {
        val parts = rawParts map { case t@Literal(Constant(const: String)) => (const, t.pos) }

        val b = new StringBuilder
        parts.zipWithIndex.foreach {
          case ((raw, _), index) => {
            if (index > 0) {
              c.abort(c.enclosingPosition, "Path interpolation can only contain string literals. Use Path.parse for runtime parsing.")
            }
            b.append(raw)
          }
        }
        val path = Path.parse(b.toString())
        c.Expr[Path](q"Path(List(..${path.parts}))")
      }
      case _ => c.abort(c.enclosingPosition, "Bad usage of Path interpolation.")
    }
  }

  def interpolateURL(c: blackbox.Context)(args: c.Expr[Any]*): c.Expr[URL] = {
    import c.universe._

    c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) => {
        val parts = rawParts map { case t@Literal(Constant(const: String)) => (const, t.pos) }

        val b = new StringBuilder
        parts.zipWithIndex.foreach {
          case ((raw, _), index) => {
            if (index > 0) {
              c.abort(
                c.enclosingPosition,
                "URL interpolation can only contain string literals. Use URL.apply for runtime parsing."
              )
            }
            b.append(raw)
          }
        }
        val url = URL(b.toString())
        val protocol = url.protocol.scheme
        val host = url.host
        val port = url.port
        val path = url.path.decoded
        val parameters = url.parameters.entries.map(t => t._1 -> t._2.values)
        val fragment = url.fragment
        c.Expr[URL](q"URL.build(protocol = $protocol, host = $host, port = $port, path = $path, parameters = $parameters, fragment = $fragment)")
      }
      case _ => c.abort(c.enclosingPosition, "Bad usage of url interpolation.")
    }
  }
}
