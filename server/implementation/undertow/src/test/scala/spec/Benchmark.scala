package spec

import cats.effect.{ExitCode, IO, IOApp}
import profig.Profig
import scribe.mdc.MDC
import spice.http.{HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.StaticHttpServer
import spice.http.server.handler.HttpHandler
import spice.net._
import scribe.cats.{io => logger}
import spice.http.client.HttpClient

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.DurationInt

object Benchmark extends IOApp {
  private val count = 1_000_000
  private val concurrency = 16
  private val processed = new AtomicLong(0L)

  override def run(args: List[String]): Task[ExitCode] = {
    Profig.initConfiguration()

    for {
      _ <- server.start()
      _ <- logPeriodically()
      start <- IO.realTimeInstant
      _ <- benchmark()
      stop <- IO.realTimeInstant
      _ <- server.dispose()
      _ <- HttpClient.dispose()
      _ <- logger.info(s"Finished in ${(stop.toEpochMilli - start.toEpochMilli) / 1000.0} seconds")
    } yield ExitCode.Success
  }

  private def logPeriodically(): Task[Unit] = logRecursively().start.map(_ => ())

  private def logRecursively(): Task[Unit] = IO.sleep(10.seconds).flatMap { _ =>
    logger.info(s"Threads: ${Thread.activeCount()}, Processed: ${processed.get()}")
  }.flatMap { _ =>
    logRecursively()
  }

  private lazy val client = HttpClient
    .url(url"http://localhost:8080")

  private def benchmark(): Task[Unit] = fs2.Stream
    .fromIterator[IO]((0 until count).iterator, 512)
    .parEvalMap(concurrency) { index =>
      client.path(URLPath.parse(s"/$index")).send().map { response =>
        assert(response.status == HttpStatus.OK)
        processed.incrementAndGet()
      }
    }
    .compile
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