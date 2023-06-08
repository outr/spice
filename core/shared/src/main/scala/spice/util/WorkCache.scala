package spice.util

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}

import java.util.concurrent.ConcurrentHashMap

/**
 * WorkCache effectively operates on a Key to guarantee that two jobs for the same Key are not concurrently processed
 * and additional checks on the Key will wait for the same result.
 *
 * @tparam Key the key tied to the work
 * @tparam Result the result of the work
 */
trait WorkCache[Key, Result] {
  private val map = new ConcurrentHashMap[Key, Deferred[IO, Result]]

  /**
   * Check for a persisted result. Work that is completed by `work` should persist to be able to be retrieved by this
   * call.
   *
   * @param key the Key to look up
   * @return Some(result) if the work has already been done for this key, None otherwise to say that the work should be
   *         done.
   */
  protected def persisted(key: Key): IO[Option[Result]]

  /**
   * Execute the work to generate the Result for this Key. The contract of this method is to persist the result so that
   * it may be retrieved by the `persisted` method.
   *
   * @param key the Key to do work for
   * @return the Result of the work
   */
  protected def work(key: Key): IO[WorkResult[Result]]

  /**
   * Retrieve the Result for this Key doing work if necessary and retrieving from persisted state if available.
   */
  def apply(key: Key): IO[Result] = IO(Option(map.get(key))).flatMap {
    case Some(d) => d.get
    case None => persisted(key).flatMap {
      case Some(result) => IO.pure(result)
      case None => Deferred[IO, Result].flatMap { d =>
        map.put(key, d)
        work(key)
          .flatMap {
            case WorkResult.FinalResult(result) =>
              d.complete(result).map { _ =>
                map.remove(key)
                result
              }
            case WorkResult.ProgressiveResult(result, complete) =>
              complete
                .flatMap { result =>
                  d.complete(result).map { _ =>
                    map.remove(key)
                  }
                }
                .handleError(errorHandler(key, _))
                .unsafeRunAndForget()
              IO.pure(result)
          }
          .handleError(errorHandler(key, _))
      }
    }
  }

  protected def errorHandler(key: Key, throwable: Throwable): Result = {
    scribe.error(s"Error while processing $key ", throwable)
    throw throwable
  }
}