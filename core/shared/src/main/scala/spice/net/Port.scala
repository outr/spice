package spice.net

import fabric.Json
import fabric.define.DefType
import fabric.rw._

case class Port private(value: Int)

object Port {
  implicit val rw: RW[Port] = RW.from[Port](
    r = _.value.json,
    w = (json: Json) => Port.fromInt(json.asInt).getOrElse(throw new RuntimeException(s"Invalid port: $json")),
    d = DefType.Int
  )

  val MinValue = 0
  val MaxValue = 65535

  def fromInt(i: Int): Option[Port] = if (i < MinValue || i > MaxValue) {
    None
  } else {
    Some(new Port(i))
  }
}