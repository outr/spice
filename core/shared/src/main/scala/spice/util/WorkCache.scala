package spice.util

import rapid.{Fiber, Task}

import java.util.concurrent.ConcurrentHashMap

/**
 * WorkCache effectively operates on a Key to guarantee that two jobs for the same Key are not concurrently processed
 * and additional checks on the Key will wait for the same result.
 *
 * @tparam Key the key tied to the work
 * @tparam Result the result of the work
 */
trait WorkCache[Key, Result] {
  private val map = new ConcurrentHashMap[Key, Task[Result]]

  /**
   * Check for a persisted result. Work that is completed by `work` should persist to be able to be retrieved by this
   * call.
   *
   * @param key the Key to look up
   * @return Some(result) if the work has already been done for this key, None otherwise to say that the work should be
   *         done.
   */
  protected def persisted(key: Key): Task[Option[Result]]

  /**
   * Execute the work to generate the Result for this Key. The contract of this method is to persist the result so that
   * it may be retrieved by the `persisted` method.
   *
   * @param key the Key to do work for
   * @return the Result of the work
   */
  protected def work(key: Key): Task[WorkResult[Result]]

  /**
   * Retrieve the Result for this Key doing work if necessary and retrieving from persisted state if available.
   */
  def apply(key: Key): Task[Result] = Task(Option(map.get(key))).flatMap {
    case Some(f) => f
    case None => persisted(key).flatMap {
      case Some(result) => Task.pure(result)
      case None => {
        val completable = Task.completable[Result]
        map.put(key, completable)
        work(key)
          .flatMap {
            case WorkResult.FinalResult(result) =>
              completable.success(result)
              map.remove(key)
              Task.pure(result)
            case WorkResult.ProgressiveResult(result, complete) =>
              complete
                .map { result =>
                  completable.success(result)
                  map.remove(key)
                }
                .handleError(throwable => Task(errorHandler(key, throwable).start()))
                .start()
              Task.pure(result)
          }
          .handleError(errorHandler(key, _))
      }
    }
  }

  protected def errorHandler(key: Key, throwable: Throwable): Task[Result] = Task {
    scribe.error(s"Error while processing $key ", throwable)
    throw throwable
  }
}