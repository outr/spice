package spice

/**
  * Used to signify that the message can be propagated up to the user and is not exposing internal information.
  */
case class UserException(message: String,
                         code: Option[Int] = None,
                         `type`: ExceptionType = ExceptionType.Info,
                         cause: Throwable = null) extends RuntimeException(message, cause)