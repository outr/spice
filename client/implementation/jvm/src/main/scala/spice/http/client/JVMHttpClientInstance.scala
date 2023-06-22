package spice.http.client

import cats.effect.IO
import spice.http.content.StreamContent
import spice.http.{Headers, HttpMethod, HttpRequest, HttpResponse, HttpStatus, WebSocket}
import spice.net.{ContentType, URL}

import java.io.InputStream
import java.net.http.HttpClient.{Redirect, Version}
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import scala.util.{Success, Try}
import java.net.{InetSocketAddress, ProxySelector, URI, http => jvm}
import java.time.Duration
import java.util.concurrent.{CompletableFuture, CompletionException}
import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._

class JVMHttpClientInstance(client: HttpClient) extends HttpClientInstance {
  implicit class CompletableFutureExtras[T](cf: CompletableFuture[T]) {
    def toIO: IO[T] = IO.fromFuture(IO(cf.asScala)).recover {
      case exc: CompletionException => throw exc.getCause
    }
  }

  private lazy val proxy = client.proxy match {
    case Some(p) => ProxySelector.of(new InetSocketAddress(p.host, p.port))
    case None => ProxySelector.getDefault
  }
  private lazy val jvmClient = jvm.HttpClient.newBuilder()
    .version(Version.HTTP_2)
    .followRedirects(Redirect.NORMAL)
    .connectTimeout(Duration.ofMillis(client.timeout.toMillis))
    .proxy(proxy)
    .build()

  override def send(request: HttpRequest): IO[Try[HttpResponse]] = for {
    jvmRequest <- request2JVM(request)
    jvmResponse <- jvmClient.sendAsync(jvmRequest, BodyHandlers.ofInputStream()).toIO
    response <- response2Spice(jvmResponse)
  } yield Success(response)

  override def webSocket(url: URL): WebSocket = throw new UnsupportedOperationException("WebSockets not supported on JVM implementation")

  override def dispose(): IO[Unit] = IO.unit

  private def request2JVM(request: HttpRequest): IO[jvm.HttpRequest] = for {
    bodyPublisher <- request.content match {
      case Some(content) => content.asStream.through(fs2.io.toInputStream).map { inputStream =>
        BodyPublishers.ofInputStream(() => inputStream)
      }.compile.lastOrError
      case None => IO.pure(null)
    }
    jvmRequest <- IO.blocking {
      val b = jvm.HttpRequest.newBuilder()
        .uri(URI.create(request.url.toString))
        .timeout(Duration.ofMillis(client.timeout.toMillis))
        .headers(request.headers.map.toList.flatMap {
          case (key, values) => values.flatMap(value => List(key, value))
        }: _*)
      val builder = request.method match {
        case HttpMethod.Get => b.GET()
        case HttpMethod.Put => b.PUT(bodyPublisher)
        case HttpMethod.Head => b.HEAD()
        case HttpMethod.Delete => b.DELETE()
        case HttpMethod.Post => b.POST(bodyPublisher)
        case m => throw new UnsupportedOperationException(s"$m not supported!")
      }
      builder.build()
    }
  } yield jvmRequest

  private def response2Spice(jvmResponse: jvm.HttpResponse[InputStream]): IO[HttpResponse] = IO.blocking {
    val headers = Headers(jvmResponse.headers().map().asScala.map {
      case (key, list) => key -> list.asScala.toList
    }.toMap)
    val contentType = Headers.`Content-Type`.value(headers).getOrElse(ContentType.`text/plain`)
    val lastModified = Headers.Response.`Last-Modified`.value(headers).getOrElse(System.currentTimeMillis())
    val content = Option(jvmResponse.body()).map { inputStream =>
      StreamContent(
        stream = fs2.io.readInputStream[IO](IO.pure(inputStream), 512),
        contentType = contentType,
        lastModified = lastModified
      )
    }
    HttpResponse(
      status = HttpStatus.byCode(jvmResponse.statusCode()),
      headers = headers,
      content = content
    )
  }
}