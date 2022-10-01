package spice.http.server.undertow

import cats.effect.{ExitCode, IO, IOApp}
import moduload.Moduload
import profig.Profig
import spice.http.content.Content
import spice.http.{HttpExchange, StreamContent}
import spice.http.server.HttpServer
import spice.net.ContentType

object Test extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    Profig.initConfiguration()

    val server = new HttpServer {
      override def handle(exchange: HttpExchange): IO[HttpExchange] = {
        exchange.modify { response =>
          IO {
//            response.withContent(Content.string("Hello, Spice! It's nice!", ContentType.`text/plain`))
            response.withContent(
              StreamContent(
                fs2.Stream.fromIterator[IO]("This is a test of streaming!".toList.map(_.toByte).iterator, 512),
                ContentType.`text/plain`
              )
            )
          }
        }
      }
    }
    server.start().flatMap { _ =>
      scribe.info("Server started!")
      server.whileRunning().map(_ => ExitCode.Success)
    }
  }
}