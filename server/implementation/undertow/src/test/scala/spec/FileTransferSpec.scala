package spec

import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.concurrent.Eventually.*
import org.scalatest.time.{Seconds, Span}
import profig.Profig
import rapid.*
import spice.http.client.HttpClient
import spice.http.server.MutableHttpServer
import spice.http.server.config.HttpServerListener
import spice.http.server.dsl.*
import spice.http.server.dsl.given
import spice.http.durable.*
import spice.net.*

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*

case class FileMeta(title: String, kind: String)
object FileMeta {
  given rw: RW[FileMeta] = RW.gen
}

/**
 * Chunked binary file transfer over a real Undertow-backed DurableSocket: upload (client → server),
 * download (server → client), and resume after a mid-transfer reconnect.
 */
class FileTransferSpec extends AnyWordSpec with Matchers {
  private val testConfig: DurableSocketConfig = DurableSocketConfig(
    ackBatchDelay = 50.millis,
    ackBatchCount = 3,
    reconnectStrategy = ReconnectStrategy.none
  )
  // Small frames + tiny window so even a modest file produces many chunks (exercises the credit
  // window and gives a reconnect a real chance to land mid-transfer).
  private val fileConfig = FileTransferConfig(frameSize = 4096, windowChunks = 4, ackEvery = 2)

  private val eventLog = new InMemoryEventLog[String, ChatEvent]
  private val durableServer = new DurableSocketServer[String, ChatEvent, ConnectInfo](
    config = testConfig,
    eventLog = eventLog,
    resolveChannel = (_, info) => Task.pure(info.room),
    fileTransfer = fileConfig
  )

  object server extends MutableHttpServer
  private def serverPort: Int = server.config.listeners().head.port.getOrElse(0)

  private def newClient(room: String, userId: String): DurableSocketClient[String, ChatEvent, ConnectInfo] =
    new DurableSocketClient[String, ChatEvent, ConnectInfo](
      createWebSocket = () => HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/wsf")).webSocket(),
      config = testConfig,
      outboundLog = eventLog,
      initialChannelId = userId,
      info = ConnectInfo(userId, room),
      clientId = userId,
      fileTransfer = fileConfig
    )

  private def randomFile(size: Int, seed: Long): Path = {
    val bytes = new Array[Byte](size)
    new scala.util.Random(seed).nextBytes(bytes)
    val tmp = Files.createTempFile("xfer-src-", ".bin")
    Files.write(tmp, bytes)
    tmp
  }

  "DurableSocket file transfer" should {
    "start the server" in {
      Profig.initConfiguration()
      server.config.clearListeners().addListeners(HttpServerListener(port = None))
      server.handler(List(path"/wsf" / durableServer))
      server.start().sync()
      server.isRunning should be(true)
    }

    "upload a large file from client to server, chunked and reassembled" in {
      val client = newClient("up-room", "uploader")

      @volatile var received: Option[ReceivedFile[FileMeta]] = None
      durableServer.onSession.attach { session =>
        session.protocol.files[FileMeta].onFile.attach { rf => received = Some(rf) }
      }
      client.connect().sync()
      eventually(timeout(Span(5, Seconds))) {
        durableServer.session("uploader") should not be empty
      }

      val src = randomFile(250_000, seed = 1L)
      val transferId = client.files[FileMeta].send(src, FileMeta("design.zip", "bundle")).sync()
      transferId.length should be > 0

      eventually(timeout(Span(20, Seconds))) {
        received should not be empty
      }
      val rf = received.get
      rf.value should be(FileMeta("design.zip", "bundle"))
      // The intrinsic filename header derives from the sent path; the app's own title rides `value`.
      rf.fileName should be(Some(src.getFileName.toString))
      java.util.Arrays.equals(Files.readAllBytes(rf.path), Files.readAllBytes(src)) should be(true)

      client.close()
    }

    "download a file from server to client" in {
      val client = newClient("down-room", "downloader")

      @volatile var received: Option[ReceivedFile[FileMeta]] = None
      client.files[FileMeta].onFile.attach { rf => received = Some(rf) }
      client.connect().sync()
      eventually(timeout(Span(5, Seconds))) {
        durableServer.session("downloader") should not be empty
      }

      val src = randomFile(180_000, seed = 2L)
      val session = durableServer.session("downloader").get
      session.protocol.files[FileMeta].send(src, FileMeta("preview.png", "image")).sync()

      eventually(timeout(Span(20, Seconds))) {
        received should not be empty
      }
      val rf = received.get
      rf.value should be(FileMeta("preview.png", "image"))
      java.util.Arrays.equals(Files.readAllBytes(rf.path), Files.readAllBytes(src)) should be(true)

      client.close()
    }

    "resume a transfer interrupted by a reconnect" in {
      val userId = "resumer"
      val client = newClient("resume-room", userId)

      @volatile var received: Option[ReceivedFile[FileMeta]] = None
      durableServer.onSession.attach { session =>
        session.protocol.files[FileMeta].onFile.attach { rf =>
          if (rf.value.title == "big.bin") received = Some(rf)
        }
      }
      client.connect().sync()
      eventually(timeout(Span(5, Seconds))) {
        durableServer.session(userId) should not be empty
      }

      val src = randomFile(600_000, seed = 3L) // ~147 chunks at 4KB
      // Kick off the send on a fiber so we can yank the connection mid-flight.
      val sendTask = client.files[FileMeta].send(src, FileMeta("big.bin", "blob")).start()

      // Drop the client's socket shortly after the transfer begins, then resume on a fresh ws.
      Thread.sleep(60)
      client.protocol.unbind()
      val lastSeq = client.protocol.highestProcessedSeq
      val ws2 = HttpClient.url(url"ws://localhost".withPort(serverPort).withPath(path"/wsf")).webSocket()
      ws2.connect().sync()
      client.protocol.bind(ws2)
      client.protocol.sendResume(userId, lastSeq, ConnectInfo(userId, "resume-room"))

      // The transfer must finish despite the interruption and the bytes must be intact.
      sendTask.sync()
      eventually(timeout(Span(30, Seconds))) {
        received should not be empty
      }
      java.util.Arrays.equals(Files.readAllBytes(received.get.path), Files.readAllBytes(src)) should be(true)

      client.close()
    }

    "stop the server" in {
      server.stop().sync()
      server.isRunning should be(false)
    }
  }
}
