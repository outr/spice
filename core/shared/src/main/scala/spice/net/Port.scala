package spice.net

import fabric.Json
import fabric.rw._

case class Port private(value: Int)

object Port {
  implicit val rw: RW[Port] = RW[Port](
    r = _.value.json,
    w = (json: Json) => Port.fromInt(json.asInt).getOrElse(throw new RuntimeException(s"Invalid port: $json"))
  )

  val MinValue = 0
  val MaxValue = 65535

  def fromInt(i: Int): Option[Port] = if (i < MinValue || i > MaxValue) {
    None
  } else {
    Some(new Port(i))
  }
}