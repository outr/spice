package spice.util

import rapid._

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration._
import scribe.{rapid => logger}

case class BufferManager(checkEvery: FiniteDuration = 10.seconds,
                         triggerAfter: Int = 100,
                         maxPerBatch: Int = 5000,
                         checkFrequency: FiniteDuration = 1.second,
                         sendEmpty: Boolean = false,
                         logErrorAfter: Int = 3) {
  private var queues = List.empty[BufferQueue[_]]
  private val lastCheck = new AtomicLong(System.currentTimeMillis())
  private var keepAlive = true

  def start: Fiber[Unit] = recurse(0).start()

  def stop(): Task[Unit] = Task {
    keepAlive = false
  }

  def create[T](handler: List[T] => Task[Unit]): BufferQueue[T] = synchronized {
    val q = BufferQueue[T](this, handler)
    queues = q :: queues
    q
  }

  private def recurse(failures: Int): Task[Unit] = Task
    .sleep(checkFrequency)
    .flatMap { _ =>
      val timeElapsed: Boolean = lastCheck.get() + checkEvery.toMillis < System.currentTimeMillis()
      queues
        .filter(q => q.nonEmpty && (timeElapsed || q.ready))
        .map(q => q.process())
        .tasks
        .map { _ =>
          if (timeElapsed) lastCheck.set(System.currentTimeMillis())
        }
        .flatMap(_ => recurse(0))
        .when(keepAlive)
    }
    .handleError { throwable =>
      val message = s"An error occurred processing the buffer (failure count: $failures). Delaying before trying again."
      val log = if (failures < logErrorAfter) {
        logger.warn(message)
      } else {
        logger.error(message, throwable)
      }
      log
        .flatMap { _ =>
          Task.sleep(checkEvery).flatMap(_ => recurse(failures + 1)).when(keepAlive)
        }
    }
}