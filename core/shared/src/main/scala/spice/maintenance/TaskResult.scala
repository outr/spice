package spice.maintenance

import scala.concurrent.duration.FiniteDuration

sealed trait TaskResult

object TaskResult {
  /**
   * Should continue on the normal schedule.
   */
  case object Continue extends TaskResult

  /**
   * Should stop and not run again.
   */
  case object Stop extends TaskResult

  /**
   * Should run again immediately.
   */
  case object RunAgain extends TaskResult

  /**
   * Configures a new scheduling delay for the next and all future schedulings.
   */
  case class ChangeSchedule(delay: () => FiniteDuration) extends TaskResult
  object ChangeSchedule {
    def to(delay: => FiniteDuration): ChangeSchedule = new ChangeSchedule(() => delay)
  }

  /**
   * Schedules the next run on a new schedule and then will return to the normal schedule.
   */
  case class NextSchedule(delay: FiniteDuration) extends TaskResult
}