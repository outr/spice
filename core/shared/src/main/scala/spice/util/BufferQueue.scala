package spice.util

import cats.effect.IO

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

case class BufferQueue[T](manager: BufferManager, handler: List[T] => IO[Unit]) {
  private val queue = new ConcurrentLinkedQueue[T]
  private val size = new AtomicInteger(0)

  def enqueue(item: T): Int = {
    queue.add(item)
    size.incrementAndGet()
  }

  def enqueue(items: List[T]): Int = items.map(enqueue).last

  def ready: Boolean = size.get() > manager.triggerAfter

  def isEmpty: Boolean = queue.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def process(): IO[Unit] = {
    val items = pollQueue(Nil, manager.maxPerBatch)
    if (manager.sendEmpty || items.nonEmpty) {
      handler(items)
    } else {
      IO.unit
    }
  }

  @tailrec
  private def pollQueue(list: List[T], remaining: Int): List[T] = if (remaining <= 0 || queue.isEmpty) {
    list.reverse
  } else {
    val item = queue.poll()
    size.decrementAndGet()
    pollQueue(item :: list, remaining - 1)
  }
}