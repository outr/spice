package spice.http.durable

import java.util.concurrent.atomic.AtomicLong

class SequenceTracker(config: DurableSocketConfig) {
  private val processedSeq = new AtomicLong(0L)
  private val unackedCount = new AtomicLong(0L)

  def highestProcessedSeq: Long = processedSeq.get()

  def acceptInbound(seq: Long): Boolean = {
    val current = processedSeq.get()
    if (seq <= current) false
    else {
      processedSeq.set(seq)
      unackedCount.incrementAndGet()
      true
    }
  }

  def shouldSendAck: Boolean = unackedCount.get() >= config.ackBatchCount

  def resetAckCount(): Unit = unackedCount.set(0L)

  def reset(toSeq: Long = 0L): Unit = {
    processedSeq.set(toSeq)
    unackedCount.set(0L)
  }
}
