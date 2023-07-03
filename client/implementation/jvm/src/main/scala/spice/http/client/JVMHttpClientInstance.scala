package spice.http.client

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import spice.http.content.{BytesContent, StreamContent, StringContent}
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
  private lazy val proxy = client.proxy match {
    case Some(p) => ProxySelector.of(new InetSocketAddress(p.host, p.port))
    case None => ProxySelector.getDefault
  }
  private[client] lazy val jvmClient = jvm.HttpClient.newBuilder()
    .version(Version.HTTP_2)
    .followRedirects(Redirect.NORMAL)
    .connectTimeout(Duration.ofMillis(client.timeout.toMillis))
    .proxy(proxy)
    .build()

  override def send(request: HttpRequest): IO[Try[HttpResponse]] = for {
    jvmRequest <- request2JVM(request)
    jvmResponse <- jvmClient.sendAsync(jvmRequest, BodyHandlers.ofByteArray()).toIO
    response <- response2Spice(jvmResponse)
  } yield Success(response)

  override def webSocket(url: URL): WebSocket = new JVMHttpClientWebSocket(url, this)

  override def dispose(): IO[Unit] = IO.unit

  private def request2JVM(request: HttpRequest): IO[jvm.HttpRequest] = for {
    // TODO: Investigate streaming through InputStream
    bodyPublisher <- request.content match {
      case Some(content: StringContent) => IO.blocking(BodyPublishers.ofString(content.value))
      case Some(content) => content.asStream.compile.toList.map(_.toArray).map { bytes =>
        BodyPublishers.ofByteArray(bytes)
      }
      case None => IO.pure(BodyPublishers.noBody())
    }
    jvmRequest <- IO.blocking {
      val builder = jvm.HttpRequest.newBuilder()
        .uri(URI.create(request.url.toString))
        .timeout(Duration.ofMillis(client.timeout.toMillis))
        .method(request.method.value, bodyPublisher)
      request.content.foreach { content =>
        builder.header("Content-Type", content.contentType.outputString)
      }
      builder.headers(request.headers.map.toList.flatMap {
          case (key, values) => values.flatMap(value => List(key, value))
        }: _*)
      builder.build()
    }
  } yield jvmRequest

  private def response2Spice(jvmResponse: jvm.HttpResponse[Array[Byte]]): IO[HttpResponse] = IO.blocking {
    val headers = Headers(jvmResponse.headers().map().asScala.map {
      case (key, list) => key -> list.asScala.toList
    }.toMap)
    val contentType = Headers.`Content-Type`.value(headers).getOrElse(ContentType.`text/plain`)
    val lastModified = Headers.Response.`Last-Modified`.value(headers).getOrElse(System.currentTimeMillis())
    // TODO: Investigate streaming through InputStream
    val content = Option(jvmResponse.body()) match {
      case Some(bytes) if bytes.nonEmpty => Some(BytesContent(bytes, contentType, lastModified))
      case _ => None
    }
    HttpResponse(
      status = HttpStatus.byCode(jvmResponse.statusCode()),
      headers = headers,
      content = content
    )
  }
}