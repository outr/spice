package spice.http

import fabric._
import fabric.define.DefType
import fabric.rw._
import spice.UserException

sealed abstract class HttpMethod private(val value: String) {
  HttpMethod.map += value -> this

  def unapply(exchange: HttpExchange): Option[HttpExchange] = if (exchange.request.method == this) {
    Some(exchange)
  } else {
    None
  }

  override def toString: String = value
}

object HttpMethod {
  private var map = Map.empty[String, HttpMethod]

  implicit val rw: RW[HttpMethod] = RW.from(
    r = m => m.value,
    w = v => apply(v.asString),
    d = DefType.Str
  )

  val Get: HttpMethod = new HttpMethod("GET") {}
  val Put: HttpMethod = new HttpMethod("PUT") {}
  val Trace: HttpMethod = new HttpMethod("TRACE") {}
  val Connect: HttpMethod = new HttpMethod("CONNECT") {}
  val Head: HttpMethod = new HttpMethod("HEAD") {}
  val Delete: HttpMethod = new HttpMethod("DELETE") {}
  val Patch: HttpMethod = new HttpMethod("PATCH") {}
  val Post: HttpMethod = new HttpMethod("POST") {}
  val Options: HttpMethod = new HttpMethod("OPTIONS") {}

  def get(value: String): Option[HttpMethod] = map.get(value.toUpperCase)
  def apply(value: String): HttpMethod = get(value).getOrElse(throw UserException(s"$value is an invalid Method."))
}