package spec

import java.io.{ByteArrayOutputStream, InputStream}
import java.net.{ServerSocket, Socket}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.util.Try

/** A local server that records each request's method, raw headers (names lower-cased) and body, and answers each
  * with an empty 200 on a connection it then closes. */
class HeaderCaptureServer {
  private val server = new ServerSocket(0)
  private val captured = new LinkedBlockingQueue[HeaderCaptureServer.Captured]

  val port: Int = server.getLocalPort

  private val thread = new Thread(() => {
    while (!server.isClosed) {
      Try(server.accept()).foreach { socket =>
        Try(handle(socket))
        Try(socket.close())
      }
    }
  })
  thread.setDaemon(true)
  thread.start()

  /** The next request the server received, waiting up to five seconds for it. */
  def next(): HeaderCaptureServer.Captured = Option(captured.poll(5L, TimeUnit.SECONDS))
    .getOrElse(throw new RuntimeException("No request received"))

  def close(): Unit = Try(server.close())

  private def handle(socket: Socket): Unit = {
    val in = socket.getInputStream
    val head = readHead(in)
    val lines = head.split("\r\n").toList
    val method = lines.head.takeWhile(_ != ' ')
    val headers = lines.tail.filter(_.contains(':')).map { line =>
      val i = line.indexOf(':')
      line.substring(0, i).trim.toLowerCase -> line.substring(i + 1).trim
    }.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
    val length = headers.get("content-length").flatMap(_.headOption).map(_.toInt).getOrElse(0)
    val body = in.readNBytes(length)
    captured.put(HeaderCaptureServer.Captured(method, headers, body))
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

object HeaderCaptureServer {
  case class Captured(method: String, headers: Map[String, List[String]], body: Array[Byte]) {
    def contentLength: Option[List[String]] = headers.get("content-length")
    def transferEncoding: Option[List[String]] = headers.get("transfer-encoding")
  }
}
