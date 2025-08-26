package spice

/**
 * Used to signify that the message can be propagated up to the user and is not exposing internal information.
 */
class UserException private(val message: String,
                            val code: Option[Int],
                            val `type`: ExceptionType,
                            val cause: Throwable,
                            val caller: String) extends RuntimeException(message, cause) {
  lazy val fullMessage: String = s"$message - $caller"
}

object UserException {
  def apply(message: String,
            code: Option[Int] = None,
            `type`: ExceptionType = ExceptionType.Warn,
            cause: Throwable = null)(implicit file: sourcecode.File,
                                     line: sourcecode.Line,
                                     enclosing: sourcecode.Enclosing): UserException = {
    val caller = s"${file.value}:${line.value} (${enclosing.value})"
    new UserException(
      message = message,
      code = code,
      `type` = `type`,
      cause = cause,
      caller = caller
    )
  }
}