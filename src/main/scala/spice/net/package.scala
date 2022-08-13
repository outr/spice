package spice

import org.typelevel.literally.Literally
import spice.net.PortLiteral

package object net {
  extension (inline ctx: StringContext)
    inline def port(inline args: Any*): Port =
      ${PortLiteral('ctx, 'args)}
    inline def ip(inline args: Any*): IP =
      ${IPLiteral('ctx, 'args)}
    inline def path(inline args: Any*): Path =
      ${PathLiteral('ctx, 'args)}

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

  object PathLiteral extends Literally[Path]:
    def validate(s: String)(using Quotes): Either[String, Expr[Path]] =
      Right('{Path.parse(${Expr(s)})})
}