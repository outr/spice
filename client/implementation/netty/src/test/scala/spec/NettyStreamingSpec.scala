package spec

import moduload.Moduload
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import rapid.*
import spice.http.client.HttpClient
import spice.http.content.StringContent
import spice.net.*

/** Tests that HttpClient.streamLines() delivers SSE lines incrementally.
  * Requires llama.cpp running on localhost:8081. */
class NettyStreamingSpec extends AnyWordSpec with Matchers {

  "HttpClient.streamLines()" should {
    "load Moduload" in {
      Moduload.load()
    }

    "stream SSE lines from llama.cpp token-by-token, not buffered" in {
      // Check if llama.cpp is available
      val available = try {
        HttpClient.url(url"http://localhost:8081/v1/models").get.send().sync()
        true
      } catch {
        case _: Throwable => false
      }

      if (!available) {
        println("llama.cpp not available on localhost:8081, skipping")
        succeed
      } else {
        val body = """{"model":"qwen3.5-9b-q4_k_m.gguf","messages":[{"role":"user","content":"Count slowly from 1 to 10, one number per line."}],"max_tokens":80,"stream":true,"chat_template_kwargs":{"enable_thinking":false}}"""

        val lineStream = HttpClient
          .url(url"http://localhost:8081/v1/chat/completions")
          .post
          .content(StringContent(body, ContentType.`application/json`))
          .streamLines()
          .sync()

        var timestamps: List[Long] = Nil

        lineStream.evalMap { line =>
          Task {
            if (line.startsWith("data: ") && !line.contains("[DONE]")) {
              timestamps = timestamps :+ System.currentTimeMillis()
            }
          }
        }.drain.sync()

        println(s"Received ${timestamps.size} SSE chunks from llama.cpp")
        timestamps.size should be > 2

        val totalSpan = timestamps.last - timestamps.head
        println(s"Time span: ${totalSpan}ms")
        timestamps.sliding(2).zipWithIndex.foreach {
          case (Seq(a, b), i) => println(s"  chunk $i -> ${i + 1}: ${b - a}ms")
          case _ => ()
        }

        // If truly streaming, tokens should be spread over >200ms
        // If buffered, all arrive within ~5ms
        println(s"Streaming: ${if (totalSpan > 200) "YES" else "NO (buffered)"}")
        totalSpan should be > 50L
      }
    }
  }
}
