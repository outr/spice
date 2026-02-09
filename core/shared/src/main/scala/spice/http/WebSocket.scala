package spice.http

import fabric.io.JsonFormatter
import rapid.Task
import reactify.{Channel, Val, Var}

import java.io.{File, FileOutputStream}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path}
import scala.util.Success
import scala.util.matching.Regex

trait WebSocket {
  private val FileUploadRegex: Regex = """fileUpload:(.+):(\d+):(.+):(.+)""".r
  
  protected val _status: Var[ConnectionStatus] = Var(ConnectionStatus.Closed)
  val status: Val[ConnectionStatus] = _status

  val send: WebSocketChannels = new WebSocketChannels
  val receive: WebSocketChannels = new WebSocketChannels
  val error: Channel[Throwable] = Channel[Throwable]

  private var uploadsEnabled = false
  
  case class FileUpload(fileName: String, length: Long, userId: String, authToken: String)

  def enableUploads(uploadDirectory: FileUpload => Option[Path]): Unit = synchronized {
    if (uploadsEnabled) {
      throw new RuntimeException("Uploads are already enabled! This should not be called more than once!")
    } else {
      var fileName: Option[String] = None
      var fileLength: Long = -1L
      var out: FileOutputStream = null
      var channel: FileChannel = null
      var written: Long = 0L

      // Listen for the file upload preamble
      receive.text.attach {
        case FileUploadRegex(name, length, userId, authToken) =>
          val upload = FileUpload(name, length.toLong, userId, authToken)
          uploadDirectory(upload) match {
            case Some(path) =>
              Files.createDirectories(path)
              fileLength = length.toLong
              fileName = Some(name)
              val file = path.resolve(name).toFile
              out = new FileOutputStream(file)
              channel = out.getChannel
              written = 0L
            case None => scribe.error(s"Invalid FileUpload: $name / $length / $userId / $authToken")
          }
        case s => scribe.warn(s"Ignoring: $s") // Ignore other messages
      }

      // Handle the actual binary data
      receive.binary.attach {
        case ByteBufferData(bb) if fileName.nonEmpty =>
          val length = bb.remaining()
          channel.write(bb)
          written += length
          if (written == fileLength) {
            scribe.info(s"$fileName Upload finished!")
            out.flush()
            out.close()
            fileName = None
            fileLength = -1L
            out = null
            channel = null
            send.text @= "fileUploaded"
          }
        case _ => // Ignore
      }
      uploadsEnabled = true
    }
  }

  /**
   * Uploads a file to the remote server using a WebSocket connection.
   *
   * Convenience feature that sends a pre-amble text of: s"fileUpload:$remoteFileName:$size:$userId:$token" and upon
   * successful receipt, waits for confirmation from the remote server of "fileUploaded". This can be handled
   * automatically by call enableFileUpload() on the server WebSocket.
   *
   * This method sends file metadata (e.g., name, size) and the file's binary data
   * to a remote server, chunk by chunk, over a WebSocket channel. It waits for
   * the server's acknowledgment before completing the process.
   *
   * @param path           the local file path to be uploaded
   * @param userId         the identifier of the user performing the upload
   * @param token          the authentication token for the user
   * @param remoteFileName the name to be assigned to the file on the remote server
   * @return a Task representing the asynchronous upload process
   */
  def upload(path: Path, userId: String, token: String, remoteFileName: String): Task[Unit] = Task.defer {
    val size = Files.size(path)
    send.text @= s"fileUpload:$remoteFileName:$size:$userId:$token"

    val buf = new Array[Byte](65536)
    val is = Files.newInputStream(path)
    try {
      var read = is.read(buf)
      while (read != -1) {
        val bb = ByteBuffer.wrap(buf, 0, read)
        send.binary @= ByteBufferData(bb)
        read = is.read(buf)
      }
    } finally {
      is.close()
    }

    val completable = Task.completable[Unit]
    receive.text.once(_ => completable.complete(Success(())), s => s == "fileUploaded")
    completable
  }

  def connect(): Task[ConnectionStatus]

  def disconnect(): Unit
}