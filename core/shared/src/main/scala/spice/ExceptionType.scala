package spice

sealed trait ExceptionType

object ExceptionType {
  /**
   * Info means that it's an informational exception that can propagate up to the user without any indications on the
   * server / application.
   */
  case object Info extends ExceptionType

  /**
   * Warn means that the message should be logged on the server / application, but not the trace.
   */
  case object Warn extends ExceptionType

  /**
   * Error means that the stack trace should be logged on the server / application.
   */
  case object Error extends ExceptionType
}