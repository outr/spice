package spec

import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import spice.http.durable.*

import scala.collection.mutable.ListBuffer

class DurableReplayAuthorizationSpec extends AnyWordSpec with Matchers {
  private class Socket extends DurableSocket[String, String, String](
    DurableSocketConfig(), new InMemoryEventLog[String, String], "channel") {
    val sent = ListBuffer.empty[(Long, String)]
    override def sendLogged(seq: Long, event: String): Unit = sent.synchronized { sent += seq -> event; () }
  }

  "durable replay authorization" should {
    "recheck both logged and held payloads, preserving sequence numbers and order" in {
      val socket = new Socket
      socket.outboundLog.append("channel", "secret").sync()
      socket.outboundLog.append("channel", "public").sync()
      socket.replayTransform = (_, event) => Task.pure(Option.when(event != "secret")(event))
      val replay = socket.replayAfter(0)
      socket.push("secret").sync()
      socket.push("later").sync()
      replay.sync()
      socket.sent.toList shouldBe List(2L -> "public", 4L -> "later")
      socket.push("live").sync()
      socket.sent.last shouldBe 5L -> "live"
    }
    "read current authorization again on a later reconnect" in {
      val socket = new Socket
      var allowed = true
      socket.outboundLog.append("channel", "private").sync()
      socket.replayTransform = (_, event) => Task.pure(Option.when(allowed)(event))
      socket.replayAfter(0).sync()
      socket.sent.toList shouldBe List(1L -> "private")
      socket.sent.clear()
      allowed = false
      socket.replayAfter(0).sync()
      socket.sent shouldBe empty
    }
    "fail closed without flushing held payloads when authorization fails" in {
      val socket = new Socket
      socket.outboundLog.append("channel", "private").sync()
      socket.replayTransform = (_, _) => Task.error(new IllegalStateException("authorization unavailable"))
      val replay = socket.replayAfter(0)
      socket.push("held secret").sync()
      intercept[IllegalStateException](replay.sync())
      socket.sent shouldBe empty
    }
  }
}
