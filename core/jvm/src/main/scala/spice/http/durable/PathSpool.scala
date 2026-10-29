package spice.http.durable

import spice.http.Headers

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path}

/** A transfer kept in a file: a temp file this side made (`owned`, deleted once let go), or a file being sent. */
class PathSpool(val path: Path, channel: FileChannel, owned: Boolean) extends FileSpool {
  override def size: Long = channel.size()

  override def write(position: Long, bytes: Array[Byte]): Unit = {
    val bb = ByteBuffer.wrap(bytes)
    while (bb.hasRemaining) channel.write(bb, position + bb.position())
  }

  override def read(position: Long, length: Int): Array[Byte] = {
    val len = math.max(0L, math.min(length.toLong, channel.size() - position)).toInt
    val arr = new Array[Byte](len)
    val bb = ByteBuffer.wrap(arr)
    var total = 0
    while (total < len) {
      val r = channel.read(bb, position + total)
      if (r < 0) total = len else total += r
    }
    arr
  }

  override def received[F](transferId: String, value: F, headers: Headers): ReceivedFile[F] = {
    try { channel.force(true); channel.close() } catch { case _: Throwable => () }
    ReceivedFile(transferId, value, headers, path)
  }

  override def release(): Unit = {
    try channel.close() catch { case _: Throwable => () }
    if (owned) try Files.deleteIfExists(path) catch { case _: Throwable => () }
  }
}
