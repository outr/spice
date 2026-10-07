package spec

import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import spice.http.{ConnectionStatus, WebSocket}
import spice.http.durable.*

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class DurableInboundReceiptSpec extends AnyWordSpec with Matchers {
  private class Wire extends WebSocket {
    override def connect(): Task[ConnectionStatus] = Task { _status @= ConnectionStatus.Open; ConnectionStatus.Open }
    override def disconnect(): Unit = _status @= ConnectionStatus.Closed
  }
  private def fixture(limit: Int = 256) = {
    val socket = new DurableSocket[String, String, String](DurableSocketConfig(ackBatchCount = 1,
      maxPendingInbound = limit), new InMemoryEventLog[String, String], "channel")
    val wire = new Wire
    val frames = new ConcurrentLinkedQueue[String]
    wire.send.text.attach(s => { frames.add(s); () })
    wire.connect().sync()
    socket.bind(wire)
    (socket, wire, frames)
  }
  private def send(wire: Wire, seq: Long, text: String): Unit =
    wire.receive.text @= s"""{"type":"event","seq":$seq,"data":"$text"}"""
  private def await(p: => Boolean): Unit = {
    val deadline = System.nanoTime() + 3.seconds.toNanos
    while (!p && System.nanoTime() < deadline) Task.sleep(10.millis).sync()
    p shouldBe true
  }

  "durable inbound receipt" should {
    "acknowledge only after receipt, in arrival order, and ignore retransmissions" in {
      val (socket, wire, frames) = fixture()
      val gate = Task.completable[Unit]
      val stored = new ConcurrentLinkedQueue[String]
      socket.inboundHandler = Some((_, event) => gate.flatMap(_ => Task { stored.add(event); () }))
      send(wire, 1, "first")
      send(wire, 2, "second")
      send(wire, 1, "duplicate")
      socket.highestProcessedSeq shouldBe 0L
      frames.asScala.mkString should not include "ack"
      gate.success(())
      await(socket.highestProcessedSeq == 2L)
      stored.asScala.toList shouldBe List("first", "second")
      await(frames.asScala.mkString.contains("ack"))
    }
    "leave a failed receipt unacknowledged and close instead of acknowledging later queued input" in {
      val (socket, wire, frames) = fixture()
      val gate = Task.completable[Unit]
      socket.inboundHandler = Some((_, _) => gate)
      send(wire, 1, "first")
      send(wire, 2, "second")
      gate.failure(new IllegalStateException("storage unavailable"))
      await(socket.state() == ProtocolState.Closed)
      socket.highestProcessedSeq shouldBe 0L
      frames.asScala.mkString should not include "ack"
    }
    "bound pending receipts without acknowledging overflow" in {
      val (socket, wire, _) = fixture(1)
      val gate = Task.completable[Unit]
      socket.inboundHandler = Some((_, _) => gate)
      send(wire, 1, "blocked")
      send(wire, 2, "overflow")
      socket.state() shouldBe ProtocolState.Closed
      socket.highestProcessedSeq shouldBe 0L
      gate.success(())
    }
    "reject an invalid pending-input limit at setup" in {
      intercept[IllegalArgumentException](DurableSocketConfig(maxPendingInbound = 0))
    }
  }
}
