package spice.util

import cats.effect.IO

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait ObjectPool[T] {
  private val _created = new AtomicInteger(0)
  private val _active = new AtomicInteger(0)
  private val _queued = new AtomicInteger(0)

  def created: Int = _created.get()
  def active: Int = _active.get()
  def queued: Int = _queued.get()

  private val queue = new ConcurrentLinkedQueue[T]

  protected def create(): IO[T]

  protected def prepareForUse(value: T): IO[T] = IO.pure(value)

  protected def resetForPool(value: T): IO[Option[T]] = IO.pure(Some(value))

  protected def dispose(value: T): IO[Unit] = IO.unit

  private def get(): IO[T] = IO {
    Option(queue.poll())
  }.flatMap {
    case Some(value) =>
      _queued.decrementAndGet()
      _active.incrementAndGet()
      prepareForUse(value)
    case None => create().flatMap { value =>
      _created.incrementAndGet()
      _active.incrementAndGet()
      prepareForUse(value)
    }
  }

  private def restore(value: T): IO[Unit] = resetForPool(value).map {
    case Some(value) =>
      queue.add(value)
      _active.decrementAndGet()
      _queued.incrementAndGet()
    case None =>
      _active.decrementAndGet()
  }

  def use[Return](f: T => IO[Return]): IO[Return] = get().flatMap { value =>
    f(value).guarantee(restore(value))
  }

  def ensureAvailable(size: Int): IO[Unit] = if (_queued.get() < size) {
    create().flatMap { value =>
      _created.incrementAndGet()
      _queued.incrementAndGet()
      queue.add(value)

      ensureAvailable(size)
    }
  } else {
    IO.unit
  }

  def waitForNoActive(delay: FiniteDuration = 100.millis): IO[Unit] = if (active == 0) {
    IO.unit
  } else {
    IO.sleep(delay).flatMap(_ => waitForNoActive(delay))
  }

  def dispose(): IO[Unit] = waitForNoActive().flatMap { _ =>
    Option(queue.poll()) match {
      case Some(value) => dispose(value).flatMap(_ => dispose())
      case None =>
        _created.set(0)
        _active.set(0)
        _queued.set(0)
        IO.unit
    }
  }
}
