package spice

/**
  * Used to signify that the message can be propagated up to the user and is not exposing internal information.
  */
case class UserException(message: String, code: Option[Int] = None) extends RuntimeException(message)