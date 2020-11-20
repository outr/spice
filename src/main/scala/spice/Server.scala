package spice

import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import moduload.Moduload
import scribe.Execution.global

import scala.util.matching.Regex

case class Server(handler: HttpHandler, implementation: ServerImplementation = Server.DefaultImplementation)

object Server {
  lazy val DefaultImplementation: ServerImplementation = {
    Moduload.load()
    ???
  }
}

trait ServerImplementation

trait HttpHandler {
  def apply(request: HttpRequest): HttpResponse
}

case class HttpRequest(method: HttpMethod = HttpMethod.Get,
                       source: IP = IP.LocalHost,
                       url: URL = URL())

case class HttpResponse()


object Test {
  def main(args: Array[String]): Unit = {
    HttpMethod.all.foreach { m =>
      scribe.info(s"Method: ${m.name}")
    }
  }
}