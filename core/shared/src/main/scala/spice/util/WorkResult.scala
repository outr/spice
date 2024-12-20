package spice.util

import rapid.Task

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
  case class ProgressiveResult[Result](result: Result, complete: Task[Result]) extends WorkResult[Result]
}