package spice.http.client

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{CharsetDecoder, CodingErrorAction, StandardCharsets}

/** Decodes UTF-8 that arrives in arbitrary chunks. The bytes of a character split across chunks are held until the
  * rest arrives, so only bytes that are malformed in the whole input become U+FFFD. */
private[client] class Utf8ChunkDecoder {
  private val decoder: CharsetDecoder = StandardCharsets.UTF_8
    .newDecoder()
    .onMalformedInput(CodingErrorAction.REPLACE)
    .onUnmappableCharacter(CodingErrorAction.REPLACE)
  private var pending: Array[Byte] = Array.emptyByteArray

  /** Text for `bytes`, excluding a trailing incomplete character, which is decoded with the next chunk. */
  def decode(bytes: Array[Byte]): String = run(pending ++ bytes, endOfInput = false)

  /** Text for any bytes still held, each incomplete character decoding as U+FFFD. */
  def finish(): String = run(pending, endOfInput = true)

  private def run(input: Array[Byte], endOfInput: Boolean): String = {
    val in = ByteBuffer.wrap(input)
    val out = CharBuffer.allocate(input.length)
    decoder.decode(in, out, endOfInput)
    if (endOfInput) {
      decoder.flush(out)
      decoder.reset()
    }
    pending = new Array[Byte](in.remaining())
    in.get(pending)
    out.flip()
    out.toString
  }
}
