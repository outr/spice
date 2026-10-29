package spice.http.durable

import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}

/** The JVM's spools: temp files, in [[FileTransferConfig.spoolDirectory]] or the system's temp directory. */
object FileSpools {
  /** A new, empty temp file, deleted when let go. `expectedSize` is what the transfer declared; a file needs no room
    * set aside. */
  def create(config: FileTransferConfig, expectedSize: Long): FileSpool = {
    val tmp = config.spoolDirectory match {
      case Some(dir) =>
        Files.createDirectories(dir)
        Files.createTempFile(dir, "xfer-", ".part")
      case None => Files.createTempFile("xfer-", ".part")
    }
    new PathSpool(tmp, FileChannel.open(tmp, StandardOpenOption.READ, StandardOpenOption.WRITE), owned = true)
  }

  /** A file to send, read where it is and left there. */
  def open(path: Path): FileSpool = new PathSpool(path, FileChannel.open(path, StandardOpenOption.READ), owned = false)
}
