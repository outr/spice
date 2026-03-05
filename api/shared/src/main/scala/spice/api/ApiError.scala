package spice.api

import fabric.rw.*

case class ApiError(message: String, code: Int = 500) extends RuntimeException(message)

object ApiError {
  given rw: RW[ApiError] = RW.gen
}
