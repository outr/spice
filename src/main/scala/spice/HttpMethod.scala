package spice

import io.circe.Decoder.Result
import io.circe._

object HttpMethod {
  lazy val all: List[HttpMethod] = List(Get, Put, Trace, Connect, Head, Delete, Patch, Post, Options)

  private lazy val map: Map[String, HttpMethod] = all.map(m => m.name.toUpperCase -> m).toMap

  implicit val encoder: Encoder[HttpMethod] = new Encoder[HttpMethod] {
    override def apply(a: HttpMethod): Json = Json.fromString(a.name)
  }
  implicit val decoder: Decoder[HttpMethod] = new Decoder[HttpMethod] {
    override def apply(c: HCursor): Result[HttpMethod] = c.value.asString match {
      case Some(HttpMethod(m)) => Right(m)
      case Some(name) => Left(DecodingFailure(s"Unable to find HttpMethod by name: $name", Nil))
      case None => Left(DecodingFailure(s"Cannot decode HttpMethod: ${c.value}", Nil))
    }
  }

  case object Get extends HttpMethod

  case object Put extends HttpMethod

  case object Trace extends HttpMethod

  case object Connect extends HttpMethod

  case object Head extends HttpMethod

  case object Delete extends HttpMethod

  case object Patch extends HttpMethod

  case object Post extends HttpMethod

  case object Options extends HttpMethod

  def get(name: String): Option[HttpMethod] = map.get(name.toUpperCase)

  def apply(name: String): HttpMethod = get(name).getOrElse(throw new RuntimeException(s"Unable to find HttpMethod by name: $name"))

  def unapply(name: String): Option[HttpMethod] = get(name)
}

sealed trait HttpMethod {
  lazy val name: String = getClass.getSimpleName.replace("$", "").toUpperCase
}