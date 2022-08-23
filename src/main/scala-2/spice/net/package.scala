package spice

import org.typelevel.literally.Literally

import scala.language.experimental.macros
import scala.util.Try

package object net {
  implicit class interpolation(val sc: StringContext) extends AnyVal {
    def port(args: Any*): Port = macro PortLiteral.make
    def ip(args: Any*): IP = macro IPLiteral.make
    def path(args: Any*): Path = macro PathLiteral.make
    def url(args: Any*): URL = macro URLLiteral.make
  }

  object PortLiteral extends Literally[Port] {
    def validate(c: Context)(s: String): Either[String, c.Expr[Port]] = {
      import c.universe.{Try => _, _}
      Try(s.toInt).toOption.flatMap(Port.fromInt) match {
        case None => Left(s"Invalid port - must be integer between ${Port.MinValue} and ${Port.MaxValue}")
        case Some(_) => Right(c.Expr(q"Port.fromInt($s.toInt).get"))
      }
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[Port] = apply(c)(args: _*)
  }

  object IPLiteral extends Literally[IP] {
    def validate(c: Context)(s: String): Either[String, c.Expr[IP]] = {
      import c.universe._
      IP.fromString(s) match {
        case None => Left(s"Invalid IP address: $s")
        case Some(_) => Right(c.Expr(q"IP.fromString($s).get"))
      }
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[IP] = apply(c)(args: _*)
  }

  object PathLiteral extends Literally[Path] {
    def validate(c: Context)(s: String): Either[String, c.Expr[Path]] = {
      import c.universe._
      Right(c.Expr(q"Path.parse($s)"))
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[Path] = apply(c)(args: _*)
  }

  object URLLiteral extends Literally[URL] {
    def validate(c: Context)(s: String): Either[String, c.Expr[URL]] = {
      import c.universe._
      URL.get(s) match {
        case Left(f) => Left(f.message)
        case Right(_) => Right(c.Expr(q"URL($s)"))
      }
    }

    def make(c: Context)(args: c.Expr[Any]*): c.Expr[URL] = apply(c)(args: _*)
  }
}