package spice.http.durable

import fabric.rw.*
import rapid.Task

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

trait EventLog[Id: RW, Event: RW] {
  def append(channelId: Id, event: Event): Task[Long]
  def replay(channelId: Id, afterSeq: Long): Task[List[(Long, Event)]]
}

class InMemoryEventLog[Id: RW, Event: RW] extends EventLog[Id, Event] {
  private val channels = new ConcurrentHashMap[Id, InMemoryEventLog.ChannelState[Event]]()

  private def getChannel(channelId: Id): InMemoryEventLog.ChannelState[Event] =
    channels.computeIfAbsent(channelId, _ => new InMemoryEventLog.ChannelState[Event])

  override def append(channelId: Id, event: Event): Task[Long] = Task {
    val channel = getChannel(channelId)
    channel.synchronized {
      val seq = channel.counter.incrementAndGet()
      channel.events = channel.events :+ (seq, event)
      seq
    }
  }

  override def replay(channelId: Id, afterSeq: Long): Task[List[(Long, Event)]] = Task {
    val channel = channels.get(channelId)
    if (channel == null) Nil
    else channel.synchronized {
      channel.events.filter(_._1 > afterSeq).toList
    }
  }
}

object InMemoryEventLog {
  private class ChannelState[Event] {
    val counter = new AtomicLong(0L)
    var events: Vector[(Long, Event)] = Vector.empty
  }
}
