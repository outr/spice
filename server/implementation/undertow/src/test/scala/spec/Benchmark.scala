package spec

import profig.Profig
import rapid._
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.StaticHttpServer
import spice.http.server.handler.HttpHandler
import spice.net._
import spice.http.client.HttpClient

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.DurationInt

object Benchmark {
  private val count = 1_000_000
  private val concurrency = 16
  private val processed = new AtomicLong(0L)

  def main(args: Array[String]): Unit = {
    Profig.initConfiguration()

    val task = for {
      _ <- server.start()
      _ <- logPeriodically()
      start <- Task.now
      _ <- benchmark()
      stop <- Task.now
      _ <- server.dispose()
      _ <- HttpClient.dispose()
      _ <- logger.info(s"Finished in ${(stop - start) / 1000.0} seconds")
    } yield ()

    task.sync()
  }

  private def logPeriodically(): Task[Unit] = logRecursively().start().map(_ => ())

  private def logRecursively(): Task[Unit] = Task.sleep(10.seconds).flatMap { _ =>
    logger.info(s"Threads: ${Thread.activeCount()}, Processed: ${processed.get()}")
  }.flatMap { _ =>
    logRecursively()
  }

  private lazy val client = HttpClient
    .url(url"http://localhost:8080")

  private def benchmark(): Task[Unit] = rapid.Stream
    .fromIterator(Task((0 until count).iterator))
    .par(concurrency) { index =>
      client.path(URLPath.parse(s"/$index")).send().map { response =>
        assert(response.status == HttpStatus.OK)
        processed.incrementAndGet()
      }
    }
    .drain

  object server extends StaticHttpServer {
    override protected val handler: HttpHandler = new HttpHandler {
      private lazy val content = Content.string("Hello, World!", ContentType.`text/plain`)

      override def handle(exchange: HttpExchange)
                         (implicit mdc: MDC): Task[HttpExchange] = exchange.modify { response =>
        Task(response.withContent(content))
      }
    }
  }
}