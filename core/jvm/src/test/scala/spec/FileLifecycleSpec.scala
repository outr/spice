package spec

import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.Task
import spice.http.{ConnectionStatus, WebSocket}
import spice.http.durable.*
import java.nio.file.Files
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class FileLifecycleSpec extends AnyWordSpec with Matchers {
  private class Wire extends WebSocket {
    override def connect(): Task[ConnectionStatus] = Task { _status @= ConnectionStatus.Open; ConnectionStatus.Open }
    override def disconnect(): Unit = _status @= ConnectionStatus.Closed
  }
  private def await(p: => Boolean): Unit = {
    val until = System.nanoTime() + 3.seconds.toNanos
    while (!p && System.nanoTime() < until) Task.sleep(5.millis).sync()
    p shouldBe true
  }
  private def fixture() = {
    val socket = new DurableSocket[String, String, String](DurableSocketConfig(), new InMemoryEventLog[String, String],
      "channel", FileTransferConfig(frameSize = 4, windowChunks = 1))
    val wire = new Wire; wire.connect().sync(); socket.bind(wire); socket.activate()
    (socket, wire, socket.files[String])
  }
  private def source() = { val p = Files.createTempFile("spice-lifecycle-", ".bin"); Files.write(p, Array.fill[Byte](16)(42)); p }

  "file transfer lifecycle" should {
    "publish the id before bytes and allow cancellation from the start callback" in {
      val (socket, wire, channel) = fixture(); val path = source()
      val started = new ConcurrentLinkedQueue[FileStartMessage]; val frames = new ConcurrentLinkedQueue[Int]
      wire.send.binary.attach(_ => { frames.add(1); () })
      channel.onSendStarted.attach(s => { started.add(s); channel.abort(s.transferId, "cancel at start") shouldBe true })
      try {
        intercept[RuntimeException](channel.send(path, "metadata").sync())
        started.size() shouldBe 1; started.peek().value.as[String] shouldBe "metadata"
        started.peek().name shouldBe path.getFileName.toString
        frames.size() shouldBe 0
        channel.abort(started.peek().transferId) shouldBe false
      } finally { socket.close(); Files.deleteIfExists(path) }
    }
    "cancel a credit-blocked sender without cancelling a second transfer" in {
      val (socket, wire, channel) = fixture(); val path = source()
      val started = new ConcurrentLinkedQueue[FileStartMessage]; val frames = new ConcurrentLinkedQueue[Int]
      channel.onSendStarted.attach(s => { started.add(s); () })
      wire.send.binary.attach(_ => { frames.add(1); () })
      val first = Task.completable[Either[Throwable, String]]; val second = Task.completable[Either[Throwable, String]]
      def send(value: String, done: rapid.task.Completable[Either[Throwable, String]]): Unit =
        channel.send(path, value).map(id => { done.success(Right(id)); () })
          .handleError(t => Task { done.success(Left(t)); () }).start()
      try {
        send("first", first); await(started.size() == 1 && frames.size() == 1)
        send("second", second); await(started.size() == 2 && frames.size() == 2)
        val ids = started.asScala.map(s => s.value.as[String] -> s.transferId).toMap
        channel.abort(ids("first"), "cancel one") shouldBe true
        first.sync().isLeft shouldBe true
        for (i <- 0 until 4) {
          wire.receive.text @= s"""{"type":"file-ack","transferId":"${ids("second")}","throughIndex":$i}"""
          if (i < 3) await(frames.size() >= i + 3)
        }
        second.sync() shouldBe Right(ids("second"))
        frames.size() shouldBe 5
        channel.abort("missing") shouldBe false
      } finally { socket.close(); Files.deleteIfExists(path) }
    }
    "abort an inbound transfer and ignore its later end frame" in {
      val (socket, wire, channel) = fixture()
      val controls = new ConcurrentLinkedQueue[String]
      val received = new ConcurrentLinkedQueue[String]
      wire.send.text.attach(s => { controls.add(s); () })
      channel.onFile.attach(f => { received.add(f.transferId); () })
      try {
        wire.receive.text @= """{"type":"file-start","transferId":"incoming","name":"test.bin","contentType":"application/octet-stream","size":4,"frameSize":4,"totalChunks":1,"value":"metadata","headers":{}}"""
        channel.abort("incoming", "not needed") shouldBe true
        channel.abort("incoming") shouldBe false
        controls.asScala.mkString should include ("file-abort")
        controls.asScala.mkString should include ("not needed")
        wire.receive.text @= """{"type":"file-end","transferId":"incoming","totalBytes":4}"""
        received.size() shouldBe 0
      } finally socket.close()
    }
    "fail a credit-blocked send when its peer aborts it" in {
      val (socket, wire, channel) = fixture(); val path = source()
      val started = new ConcurrentLinkedQueue[FileStartMessage]
      val frames = new ConcurrentLinkedQueue[Int]
      channel.onSendStarted.attach(s => { started.add(s); () })
      wire.send.binary.attach(_ => { frames.add(1); () })
      val result = Task.completable[Either[Throwable, String]]
      try {
        channel.send(path, "metadata").map(id => { result.success(Right(id)); () })
          .handleError(t => Task { result.success(Left(t)); () }).start()
        await(started.size() == 1 && frames.size() == 1)
        wire.receive.text @= s"""{"type":"file-abort","transferId":"${started.peek().transferId}","reason":"peer refused"}"""
        result.sync().isLeft shouldBe true
        channel.abort(started.peek().transferId) shouldBe false
        frames.size() shouldBe 1
      } finally { socket.close(); Files.deleteIfExists(path) }
    }
  }
}
