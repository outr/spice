package spice.http.durable

/** A browser's spools: memory, since it has no file system to write to. */
object FileSpools {
  def create(config: FileTransferConfig, expectedSize: Long): FileSpool = new MemorySpool(expectedSize)

  /** `bytes` to send, held as they are. */
  def of(bytes: Array[Byte]): FileSpool = {
    val spool = new MemorySpool(bytes.length.toLong)
    spool.write(0L, bytes)
    spool
  }
}
