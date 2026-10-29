package spice.http.durable

import spice.http.Headers

/** A transfer kept in memory, which is all a browser has: room for `expectedSize` bytes at the start, grown as needed.
  * [[FileTransferConfig.maxInboundBytes]] is what bounds an inbound one. */
class MemorySpool(expectedSize: Long) extends FileSpool {
  private var data: Array[Byte] = new Array[Byte](math.max(0L, math.min(expectedSize, Int.MaxValue.toLong)).toInt)
  private var length: Int = 0

  override def size: Long = length.toLong

  override def write(position: Long, bytes: Array[Byte]): Unit = {
    val end = position + bytes.length
    if (end > Int.MaxValue) throw new IllegalArgumentException(s"A transfer of $end bytes is too large to hold in memory")
    if (end > data.length) {
      val grown = new Array[Byte](math.max(end, math.min(data.length.toLong * 2, Int.MaxValue.toLong)).toInt)
      System.arraycopy(data, 0, grown, 0, length)
      data = grown
    }
    System.arraycopy(bytes, 0, data, position.toInt, bytes.length)
    length = math.max(length, end.toInt)
  }

  override def read(position: Long, length: Int): Array[Byte] = {
    val from = math.min(position, this.length.toLong).toInt
    java.util.Arrays.copyOfRange(data, from, math.min(this.length.toLong, from.toLong + length).toInt)
  }

  override def received[F](transferId: String, value: F, headers: Headers): ReceivedFile[F] =
    ReceivedFile(transferId, value, headers, if (data.length == length) data else java.util.Arrays.copyOf(data, length))

  override def release(): Unit = {
    data = new Array[Byte](0)
    length = 0
  }
}
