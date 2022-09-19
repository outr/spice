package spice

import cats.effect.IO

import java.util.concurrent.atomic.AtomicInteger

trait Initializable {
  private val status = new AtomicInteger(0)

  def isInitialized: Boolean = status.get() == 2

  final def init(): IO[Boolean] = if (status.compareAndSet(0, 1)) {
    initialize().map { _ =>
      status.set(2)
      true
    }
  } else {
    IO.pure(false)
  }

  protected def initialize(): IO[Unit]
}