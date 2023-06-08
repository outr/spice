package spice.util

import cats.effect.IO

sealed trait WorkResult[Result]

object WorkResult {
  /**
   * The final result. The work if finished.
   */
  case class FinalResult[Result](result: Result) extends WorkResult[Result]

  /**
   * The intermediate result. The work is partially completed, but more work needs to be done before it's fully complete.
   * The `complete` will be asynchronously executed to finish the partially completed work.
   */
  case class ProgressiveResult[Result](result: Result, complete: IO[Result]) extends WorkResult[Result]
}