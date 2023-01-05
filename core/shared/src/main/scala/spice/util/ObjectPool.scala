package spice.util

import cats.effect.IO

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger

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

  def use[Return](f: T => IO[T]): IO[T] = get().flatMap { value =>
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
}
