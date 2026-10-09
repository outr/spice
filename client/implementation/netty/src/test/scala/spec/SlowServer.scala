package spec

import java.io.{ByteArrayOutputStream, InputStream}
import java.net.{ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** A local server that records the method of each request it receives and answers it only after `delayMillis`, so a
  * client with a shorter timeout gives up on every request it sends. */
class SlowServer(delayMillis: Long) {
  private val server = new ServerSocket(0)
  private val received = new ConcurrentLinkedQueue[String]

  val port: Int = server.getLocalPort

  private val acceptor = new Thread(() => {
    while (!server.isClosed) {
      Try(server.accept()).foreach { socket =>
        val handler = new Thread(() => {
          Try(handle(socket))
          Try(socket.close())
          ()
        })
        handler.setDaemon(true)
        handler.start()
      }
    }
  })
  acceptor.setDaemon(true)
  acceptor.start()

  /** The methods of the requests received so far, in arrival order. */
  def methods: List[String] = received.asScala.toList

  def close(): Unit = Try(server.close())

  private def handle(socket: Socket): Unit = {
    val in = socket.getInputStream
    val head = readHead(in)
    val lines = head.split("\r\n").toList
    received.add(lines.head.takeWhile(_ != ' '))
    val length = lines.tail.collectFirst {
      case line if line.toLowerCase.startsWith("content-length:") => line.substring(line.indexOf(':') + 1).trim.toInt
    }.getOrElse(0)
    in.readNBytes(length)
    Thread.sleep(delayMillis)
    val out = socket.getOutputStream
    out.write("HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n".getBytes(StandardCharsets.US_ASCII))
    out.flush()
  }

  private def readHead(in: InputStream): String = {
    val buffer = new ByteArrayOutputStream
    var last4 = 0
    var b = in.read()
    while (b != -1 && last4 != 0x0d0a0d0a) {
      buffer.write(b)
      last4 = (last4 << 8) | b
      if (last4 != 0x0d0a0d0a) b = in.read()
    }
    new String(buffer.toByteArray, StandardCharsets.US_ASCII)
  }
}
