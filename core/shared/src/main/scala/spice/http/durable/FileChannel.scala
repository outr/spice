package spice.http.durable

import fabric.*
import fabric.io.JsonFormatter
import fabric.rw.*
import rapid.Task
import rapid.task.Completable
import reactify.Channel
import spice.http.Headers

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters.*

/**
 * Typed file-transfer facet on a [[DurableSocket]] (one payload type `F` per socket). The control
 * plane — start / end / ack / resume / abort — rides the durable text channel; the bytes ride the
 * binary channel as `frameSize`-sized chunks correlated by `transferId`. Concurrent transfers
 * multiplex by id. Backpressure is a credit window (`windowChunks` unacked chunks max). Transfers
 * are ephemeral relative to the durable event log; a reconnect resumes them from the receiver's
 * highest contiguous chunk rather than restarting.
 *

 * Both inbound and outbound bytes spool through a [[FileSpool]] the sender can re-read for resume: a temp file on the
 * JVM, so neither side holds a whole large file in heap, and memory in a browser, which has no file system. Sending a
 * file where it is (a `Path` on the JVM, a `File` or `Blob` in a browser) is the platform's ([[FileChannelPlatform]]).
 */
class FileChannel[F: RW](socket: DurableSocket[?, ?, ?], config: FileTransferConfig) extends FileChannelPlatform[F] {
  /** Fires once a transfer has been fully received: sealed to a temp file on the JVM, held in memory in a browser. */
  val onFile: Channel[ReceivedFile[F]] = socket.heard[ReceivedFile[F]](InboundHold.Files)
  /** Fires as bytes move in either direction. */
  val onProgress: Channel[FileProgress] = Channel[FileProgress]

  private val outbound = new ConcurrentHashMap[String, Outbound]()
  private val inbound = new ConcurrentHashMap[String, Inbound]()
  private val ids = new java.util.Random()

  // Reassembly buffer for the binary channel (length-delimited frames). Guarded because, while
  // events arrive serially per connection, a reconnect can swap the delivering thread.
  private val reasmLock = new Object
  private val reasm = new java.io.ByteArrayOutputStream()

  // start a best-effort timeout sweeper
  sweepLoop().start()

  // --- Public send API (sending a file where it is: FileChannelPlatform) ---

  def send(name: String,
           contentType: String,
           bytes: rapid.Stream[Byte],
           value: F,
           headers: Headers): Task[String] = Task.defer {
    val spool = FileSpools.create(config, 0L)
    var position = 0L
    bytes.chunk(config.frameSize).foreach { chunk =>
      val arr = chunk.toArray
      spool.write(position, arr)
      position += arr.length
    }.drain.flatMap { _ =>
      sendSpool(name, contentType, spool, value, headers)
    }.handleError { err =>
      spool.release()
      Task.error(err)
    }
  }

  def send(name: String, contentType: String, bytes: rapid.Stream[Byte], value: F): Task[String] =
    send(name, contentType, bytes, value, Headers.empty)

  // --- Outbound ---

  protected def sendSpool(name: String,
                          contentType: String,
                          spool: FileSpool,
                          value: F,
                          headers: Headers): Task[String] = Task.defer {
    // only tells this socket's transfers apart; UUID.randomUUID would need a SecureRandom, which a browser build lacks
    val transferId = new UUID(ids.nextLong(), ids.nextLong()).toString
    val size = spool.size
    val totalChunks = if (size == 0L) 0 else ((size + config.frameSize - 1) / config.frameSize).toInt
    val envelope = headers
      .setHeader("Content-Type", contentType)
      .setHeader("Content-Length", size.toString)
      .setHeader("Content-Disposition", s"""attachment; filename="$name"""")
    val t = new Outbound(transferId, spool, size, totalChunks)
    outbound.put(transferId, t)
    sendControl("file-start", FileStartMessage(
      transferId = transferId,
      name = name,
      contentType = contentType,
      size = size,
      frameSize = config.frameSize,
      totalChunks = totalChunks,
      value = value.json,
      headers = envelope.map
    ).json)
    pump(t).handleError { err =>
      abortOutbound(t, Option(err.getMessage).getOrElse("send failed"))
      Task.unit
    }.start()
    t.completion
  }

  private def pump(t: Outbound): Task[Unit] = Task.defer {
    if (t.aborted) Task.unit
    else if (socket.state() != ProtocolState.Active) t.park().flatMap(_ => pump(t))
    else if (t.cursor.get() >= t.totalChunks) {
      if (!t.endSent) {
        t.endSent = true
        sendControl("file-end", FileEndMessage(t.transferId, t.size).json)
      }
      if (t.ackedThrough.get() >= t.totalChunks - 1) completeOutbound(t)
      else t.park().flatMap(_ => pump(t))
    } else if (t.cursor.get() - t.ackedThrough.get() > config.windowChunks) {
      t.park().flatMap(_ => pump(t))
    } else {
      val idx = t.cursor.getAndIncrement()
      Task {
        val payload = readChunk(t.spool, idx, t.size)
        socket.sendBinaryRaw(frameFor(t.transferId, idx, payload))
        t.touch()
        onProgress @= FileProgress(t.transferId, math.min((idx + 1).toLong * config.frameSize, t.size), t.size)
      }.flatMap(_ => pump(t))
    }
  }

  private def completeOutbound(t: Outbound): Task[Unit] = Task {
    closeOutbound(t)
    if (!t.done) {
      t.done = true
      t.completion.success(t.transferId)
    }
  }

  private def abortOutbound(t: Outbound, reason: String): Unit = {
    if (!t.done) {
      t.done = true
      sendControl("file-abort", FileAbortMessage(t.transferId, reason).json)
      closeOutbound(t)
      t.completion.failure(new RuntimeException(s"File transfer ${t.transferId} aborted: $reason"))
    }
  }

  private def closeOutbound(t: Outbound): Unit = {
    outbound.remove(t.transferId)
    t.spool.release()
  }

  // --- Inbound ---

  private[durable] def acceptStart(json: Json): Unit = {
    val msg = json.as[FileStartMessage]
    startRefusal(msg) match {
      case Some(reason) => sendControl("file-abort", FileAbortMessage(msg.transferId, reason).json)
      case None =>
        val value = msg.value.as[F]
        val headers = Headers(msg.headers)
        val spool = FileSpools.create(config, msg.size)
        val in = new Inbound(msg.transferId, value, headers, spool, msg.size, msg.frameSize, msg.totalChunks)
        inbound.put(msg.transferId, in)
    }
  }

  /** Why a declared transfer is not accepted: over [[FileTransferConfig.maxInboundBytes]], or a shape (size, frame size,
    * chunk count) that does not describe one file, since chunks are placed by index times the declared frame size. */
  private def startRefusal(msg: FileStartMessage): Option[String] = {
    val expectedChunks = if (msg.size == 0L || msg.frameSize <= 0) 0L else (msg.size + msg.frameSize - 1) / msg.frameSize
    config.maxInboundBytes.filter(msg.size > _) match {
      case Some(max) => Some(s"file of ${msg.size} bytes is over the $max-byte limit")
      case None if msg.size < 0L || msg.frameSize <= 0 || msg.totalChunks.toLong != expectedChunks =>
        Some(s"inconsistent transfer: ${msg.size} bytes in ${msg.totalChunks} chunks of ${msg.frameSize}")
      case None => None
    }
  }

  private def abortInbound(in: Inbound, reason: String): Unit = {
    if (inbound.remove(in.transferId) != null) {
      in.spool.release()
      sendControl("file-abort", FileAbortMessage(in.transferId, reason).json)
    }
  }

  /** The binary channel is a byte stream, not a frame boundary: the ws layer may split one logical
    * frame across several `receive.binary` events (Undertow emits one per pooled buffer; proxies
    * fragment large frames) or coalesce several. Each frame is therefore length-delimited and
    * reassembled here before parsing. Binary events arrive serially on one connection thread, so a
    * single guarded accumulator suffices. */
  private[durable] def acceptChunk(bb: ByteBuffer): Unit = reasmLock.synchronized {
    val incoming = new Array[Byte](bb.remaining())
    bb.get(incoming)
    reasm.write(incoming)
    var data = reasm.toByteArray
    var offset = 0
    while (data.length - offset >= 4 && {
      val frameLen = ByteBuffer.wrap(data, offset, 4).getInt()
      data.length - offset - 4 >= frameLen
    }) {
      val frameLen = ByteBuffer.wrap(data, offset, 4).getInt()
      handleFrame(ByteBuffer.wrap(data, offset + 4, frameLen))
      offset += 4 + frameLen
    }
    reasm.reset()
    if (offset < data.length) reasm.write(data, offset, data.length - offset)
  }

  private def handleFrame(bb: ByteBuffer): Unit = {
    if (bb.remaining() < 6) return
    val idLen = bb.getShort().toInt
    if (idLen < 0 || bb.remaining() < idLen + 4) return
    val idBytes = new Array[Byte](idLen)
    bb.get(idBytes)
    val transferId = new String(idBytes, UTF_8)
    val idx = bb.getInt()
    val payload = new Array[Byte](bb.remaining())
    bb.get(payload)
    val in = inbound.get(transferId)
    if (in != null && (idx < 0 || idx >= in.totalChunks || payload.length > in.frameSize ||
        idx.toLong * in.frameSize + payload.length > in.size)) {
      abortInbound(in, s"chunk $idx of ${payload.length} bytes falls outside the declared ${in.size}-byte file")
    } else if (in != null) {
      val isNew = !in.received.get(idx)
      in.spool.write(idx.toLong * in.frameSize, payload)
      if (isNew) {
        in.received.set(idx)
        in.bytesReceived.addAndGet(payload.length.toLong)
      }
      in.touch()
      onProgress @= FileProgress(transferId, in.bytesReceived.get(), in.size)
      in.ackCounter += 1
      if (in.ackCounter >= config.ackEvery) {
        in.ackCounter = 0
        sendControl("file-ack", FileAckMessage(transferId, in.highestContiguous).json)
      }
      trySeal(transferId)
    }
  }

  private[durable] def acceptEnd(json: Json): Unit = {
    val msg = json.as[FileEndMessage]
    trySeal(msg.transferId)
    // If still pending after the end marker, chunks are missing (text `file-end` can outrun the
    // trailing binary frames) — ask the sender to resend from the first gap.
    val in = inbound.get(msg.transferId)
    if (in != null) sendControl("file-resume", FileResumeMessage(msg.transferId, in.firstMissing).json)
  }

  /** Seal a fully-received transfer exactly once: completeness is "every chunk present", checked on
    * each chunk so sealing never waits on `file-end` ordering. The `ConcurrentHashMap.remove` claim
    * guarantees a single sealer even if a chunk and the end marker race. */
  private def trySeal(transferId: String): Unit = {
    val in = inbound.get(transferId)
    if (in != null && in.highestContiguous == in.totalChunks - 1 && in.bytesReceived.get() == in.size) {
      if (inbound.remove(transferId) != null) {
        val file = in.spool.received(transferId, in.value, in.headers)
        sendControl("file-ack", FileAckMessage(transferId, in.totalChunks - 1).json)
        onFile @= file
      }
    }
  }

  private[durable] def acceptAck(json: Json): Unit = {
    val msg = json.as[FileAckMessage]
    val t = outbound.get(msg.transferId)
    if (t != null) {
      if (msg.throughIndex > t.ackedThrough.get()) t.ackedThrough.set(msg.throughIndex)
      t.touch()
      t.signal()
    }
  }

  private[durable] def acceptResume(json: Json): Unit = {
    val msg = json.as[FileResumeMessage]
    val t = outbound.get(msg.transferId)
    if (t != null) {
      t.cursor.set(msg.fromIndex)
      t.ackedThrough.set(msg.fromIndex - 1)
      t.endSent = false
      t.touch()
      t.signal()
    }
  }

  private[durable] def acceptAbort(json: Json): Unit = {
    val msg = json.as[FileAbortMessage]
    Option(outbound.get(msg.transferId)).foreach { t =>
      if (!t.done) {
        t.done = true
        closeOutbound(t)
        t.completion.failure(new RuntimeException(s"Peer aborted transfer ${msg.transferId}: ${msg.reason}"))
      }
    }
    Option(inbound.remove(msg.transferId)).foreach { in =>
      in.spool.release()
    }
  }

  /** Called by the socket when it (re)activates after a reconnect. Receivers re-request resume of
    * incomplete inbound transfers; senders wake their pumps to re-evaluate connection state. */
  private[durable] def onReactivated(): Unit = {
    // A frame part-way through arriving was lost with the old connection.
    reasmLock.synchronized(reasm.reset())
    inbound.values().asScala.foreach { in =>
      if (in.firstMissing < in.totalChunks) {
        sendControl("file-resume", FileResumeMessage(in.transferId, in.firstMissing).json)
      }
    }
    outbound.values().asScala.foreach(_.signal())
  }

  // --- Helpers ---

  private def sendControl(tpe: String, json: Json): Unit = {
    val withType = json match {
      case o: Obj => Obj(o.value + ("type" -> str(tpe)))
      case other  => obj("type" -> str(tpe), "data" -> other)
    }
    socket.sendRaw(JsonFormatter.Default(withType))
  }

  private def readChunk(spool: FileSpool, idx: Int, size: Long): Array[Byte] = {
    val pos = idx.toLong * config.frameSize
    spool.read(pos, math.min(config.frameSize.toLong, size - pos).toInt)
  }

  private def frameFor(transferId: String, idx: Int, payload: Array[Byte]): ByteBuffer = {
    val idBytes = transferId.getBytes(UTF_8)
    val body = 2 + idBytes.length + 4 + payload.length
    val bb = ByteBuffer.allocate(4 + body)
    bb.putInt(body) // length prefix so the receiver can reassemble across fragmented binary events
    bb.putShort(idBytes.length.toShort)
    bb.put(idBytes)
    bb.putInt(idx)
    bb.put(payload)
    bb.flip()
    bb
  }

  private def sweepLoop(): Task[Unit] = Task.sleep(config.transferTimeout).flatMap { _ =>
    if (socket.state() == ProtocolState.Closed) Task.unit
    else {
      val cutoff = System.currentTimeMillis() - config.transferTimeout.toMillis
      outbound.values().asScala.filter(_.lastActivity < cutoff).foreach(abortOutbound(_, "transfer timed out"))
      inbound.values().asScala.filter(_.lastActivity < cutoff).foreach { in =>
        inbound.remove(in.transferId)
        in.spool.release()
      }
      sweepLoop()
    }
  }

  // --- Per-transfer state ---

  private final class Outbound(val transferId: String,
                               val spool: FileSpool,
                               val size: Long,
                               val totalChunks: Int) {
    val cursor: AtomicInteger = new AtomicInteger(0)
    val ackedThrough: AtomicInteger = new AtomicInteger(-1)
    val completion: Completable[String] = Task.completable[String]
    @volatile var endSent: Boolean = false
    @volatile var done: Boolean = false
    @volatile var aborted: Boolean = false
    @volatile var lastActivity: Long = System.currentTimeMillis()

    private var wake: Completable[Unit] = null
    private var woken: Boolean = false

    def touch(): Unit = lastActivity = System.currentTimeMillis()

    /** Park the pump until the next [[signal]] (or return immediately if a signal is already armed). */
    def park(): Task[Unit] = synchronized {
      if (woken) { woken = false; Task.unit }
      else { val c = Task.completable[Unit]; wake = c; c }
    }

    /** Wake a parked pump, or arm a one-shot wake for the next [[park]]. */
    def signal(): Unit = synchronized {
      if (wake != null) { val c = wake; wake = null; c.success(()) }
      else woken = true
    }
  }

  private final class Inbound(val transferId: String,
                              val value: F,
                              val headers: Headers,
                              val spool: FileSpool,
                              val size: Long,
                              val frameSize: Int,
                              val totalChunks: Int) {
    val received: java.util.BitSet = new java.util.BitSet(math.max(totalChunks, 1))
    val bytesReceived: AtomicLong = new AtomicLong(0L)
    @volatile var ackCounter: Int = 0
    @volatile var lastActivity: Long = System.currentTimeMillis()

    def touch(): Unit = lastActivity = System.currentTimeMillis()

    /** Highest index `i` such that chunks `[0, i]` are all present (`-1` if chunk 0 is missing). */
    def highestContiguous: Int = received.nextClearBit(0) - 1

    /** First missing chunk index, clamped to `totalChunks` when none are missing. */
    def firstMissing: Int = received.nextClearBit(0)
  }
}
