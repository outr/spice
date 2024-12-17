package spice.util

import rapid.Task

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

  protected def create(): Task[T]

  protected def prepareForUse(value: T): Task[T] = Task.pure(value)

  protected def resetForPool(value: T): Task[Option[T]] = Task.pure(Some(value))

  protected def dispose(value: T): Task[Unit] = Task.unit

  private def get(): Task[T] = Task {
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

  private def restore(value: T): Task[Unit] = resetForPool(value).map {
    case Some(value) =>
      queue.add(value)
      _active.decrementAndGet()
      _queued.incrementAndGet()
    case None =>
      _active.decrementAndGet()
  }

  def use[Return](f: T => Task[Return]): Task[Return] = get().flatMap { value =>
    f(value).guarantee(restore(value))
  }

  def ensureAvailable(size: Int): Task[Unit] = if (_queued.get() < size) {
    create().flatMap { value =>
      _created.incrementAndGet()
      _queued.incrementAndGet()
      queue.add(value)

      ensureAvailable(size)
    }
  } else {
    Task.unit
  }

  def waitForNoActive(delay: FiniteDuration = 100.millis): Task[Unit] = if (active == 0) {
    Task.unit
  } else {
    Task.sleep(delay).flatMap(_ => waitForNoActive(delay))
  }

  def dispose(): Task[Unit] = waitForNoActive().flatMap { _ =>
    Option(queue.poll()) match {
      case Some(value) => dispose(value).flatMap(_ => dispose())
      case None =>
        _created.set(0)
        _active.set(0)
        _queued.set(0)
        Task.unit
    }
  }
}
