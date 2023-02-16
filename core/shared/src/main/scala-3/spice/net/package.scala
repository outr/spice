package spice

import org.typelevel.literally.Literally
import spice.net.PortLiteral

package object net {
  extension (inline ctx: StringContext)
    inline def port(inline args: Any*): Port =
      ${PortLiteral('ctx, 'args)}
    inline def ip(inline args: Any*): IP =
      ${IPLiteral('ctx, 'args)}
    inline def path(inline args: Any*): URLPath =
      ${URLPathLiteral('ctx, 'args)}
    inline def url(inline args: Any*): URL =
      ${URLLiteral('ctx, 'args)}
    inline def email(inline args: Any*): EmailAddress =
      ${EmailAddressLiteral('ctx, 'args)}

  object PortLiteral extends Literally[Port]:
    def validate(s: String)(using Quotes): Either[String, Expr[Port]] =
      s.toIntOption.flatMap(Port.fromInt) match
        case None => Left(s"Invalid port - must be integer between ${Port.MinValue} and ${Port.MaxValue}")
        case Some(_) => Right('{Port.fromInt(${Expr(s)}.toInt).get})

  object IPLiteral extends Literally[IP]:
    def validate(s: String)(using Quotes): Either[String, Expr[IP]] =
      IP.fromString(s) match
        case None => Left(s"Invalid IP address: $s")
        case Some(_) => Right('{IP.fromString(${Expr(s)}).get})

  object URLPathLiteral extends Literally[URLPath]:
    def validate(s: String)(using Quotes): Either[String, Expr[URLPath]] =
      Right('{URLPath.parse(${Expr(s)})})

  object URLLiteral extends Literally[URL]:
    def validate(s: String)(using Quotes): Either[String, Expr[URL]] =
      URL.get(s) match
        case Left(f) => Left(f.message)
        case Right(_) => Right('{URL.parse(${Expr(s)})})

  object EmailAddressLiteral extends Literally[EmailAddress]:
    def validate(s: String)(using Quotes): Either[String, Expr[EmailAddress]] =
      EmailAddress.parse(s) match
        case Some(e) => Right('{EmailAddress(${Expr(e.local)}, ${Expr(e.domain)})})
        case None => Left(s"$s is not a valid email address")
}