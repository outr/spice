package spec

import fabric.*
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import rapid.*
import spice.http.Headers
import spice.http.durable.*

import scala.concurrent.duration.*

/**
 * Files over a pair of durable sockets in memory, and what a socket holds until it is heard. Shared, so it runs in a
 * browser's runtime too, where the file channel holds transfers in memory (it did not link there before), once the
 * JS tests build against Scala.js artifacts: today core's dependencies are the JVM ones on both platforms.
 */
class FileChannelSpec extends AsyncWordSpec with Matchers {
  private val files = FileTransferConfig(frameSize = 1024, windowChunks = 4, ackEvery = 2)

  private def socket(): DurableSocket[String, String, String] =
    new DurableSocket[String, String, String](DurableSocketConfig(), new InMemoryEventLog[String, String], "channel", files)

  private def pair(): (DurableSocket[String, String, String], DurableSocket[String, String, String]) = {
    val (left, right) = PipeSocket.pair()
    val a = socket()
    val b = socket()
    a.bind(left)
    b.bind(right)
    a.activate()
    b.activate()
    (a, b)
  }

  private def bytes(size: Int, seed: Long): Vector[Byte] = {
    val arr = new Array[Byte](size)
    new scala.util.Random(seed).nextBytes(arr)
    arr.toVector
  }

  private def received[F](socket: DurableSocket[?, ?, ?])(using RW[F]): Task[ReceivedFile[F]] = {
    val done = Task.completable[ReceivedFile[F]]
    socket.files[F].onFile.once(file => done.success(file))
    done
  }

  "a durable socket's file channel" should {
    "send a file of many chunks, out of a stream, and deliver every byte" in {
      val (a, b) = pair()
      val content = bytes(50 * 1024 + 17, 7L)
      val arrived = received[String](b)
      a.files[String].send("report.bin", "application/x-report", Stream.emits(content), "the report").flatMap { _ =>
        arrived.flatMap(file => file.bytes.toList.map(file -> _))
      }.map { case (file, got) =>
        a.close()
        b.close()
        file.value should be("the report")
        file.fileName should be(Some("report.bin"))
        file.contentType should be(Some("application/x-report"))
        file.size should be(content.size.toLong)
        got should be(content.toList)
      }.toFuture
    }

    "send an empty file" in {
      val (a, b) = pair()
      val arrived = received[String](b)
      a.files[String].send("empty.txt", "text/plain", Stream.emits(Vector.empty[Byte]), "nothing").flatMap(_ => arrived).map { file =>
        a.close()
        b.close()
        file.size should be(0L)
      }.toFuture
    }

    "refuse a file over the receiver's limit" in {
      val (left, right) = PipeSocket.pair()
      val a = socket()
      val b = new DurableSocket[String, String, String](DurableSocketConfig(), new InMemoryEventLog[String, String],
        "channel", files.copy(maxInboundBytes = Some(4096L)))
      a.bind(left)
      b.bind(right)
      a.activate()
      b.activate()
      b.files[String]
      a.files[String].send("big.bin", "application/octet-stream", Stream.emits(bytes(8192, 3L)), "big").attempt.map { result =>
        a.close()
        b.close()
        result.isFailure should be(true)
      }.toFuture
    }
  }

  "a socket holding until heard" should {
    "deliver what came before its handlers attached, in order" in {
      val (left, right) = PipeSocket.pair()
      val a = socket()
      val b = socket()
      b.holdUntilHeard()
      a.bind(left)
      b.bind(right)
      a.activate()
      b.activate()
      a.sendEphemeral(obj("type" -> str("first")))
      a.sendEphemeral(obj("type" -> str("second")))
      a.push("an event").flatMap(_ => Task.sleep(200.millis)).flatMap { _ =>
        val ephemerals = Task.completable[List[String]]
        var heard = List.empty[String]
        b.onEphemeral.attach { json =>
          heard = heard :+ json("type").asString
          if (heard.size == 2) ephemerals.success(heard)
        }
        val event = Task.completable[String]
        b.onEvent.attach { case (_, e) => event.success(e) }
        ephemerals.flatMap(e => event.map(e -> _))
      }.map { case (ephemerals, event) =>
        a.close()
        b.close()
        ephemerals should be(List("first", "second"))
        event should be("an event")
      }.toFuture
    }

    "deliver a file whose transfer began before the file channel was asked for" in {
      val (left, right) = PipeSocket.pair()
      val a = socket()
      val b = socket()
      b.holdUntilHeard()
      a.bind(left)
      b.bind(right)
      a.activate()
      b.activate()
      val content = bytes(3000, 11L)
      a.files[String].send("late.bin", "application/octet-stream", Stream.emits(content), "late").start()
      Task.sleep(200.millis).flatMap(_ => received[String](b)).flatMap(file => file.bytes.toList).map { got =>
        a.close()
        b.close()
        got should be(content.toList)
      }.toFuture
    }

    "hold nothing unless asked to" in {
      val (a, b) = pair()
      var heard = List.empty[String]
      a.sendEphemeral(obj("type" -> str("unheard")))
      Task.sleep(200.millis).flatMap { _ =>
        b.onEphemeral.attach(json => heard = heard :+ json("type").asString)
        Task.sleep(100.millis)
      }.map { _ =>
        a.close()
        b.close()
        heard should be(Nil)
      }.toFuture
    }
  }
}
