package spice.http.client

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.apache.http.entity.mime.MultipartEntityBuilder
import spice.http.content.FormDataEntry.{FileEntry, StringEntry}
import spice.http.content.{BytesContent, FormDataContent, StreamContent, StringContent}
import spice.http.{Headers, HttpMethod, HttpRequest, HttpResponse, HttpStatus, WebSocket}
import spice.net.{ContentType, URL}

import java.io.{ByteArrayOutputStream, InputStream}
import java.net.http.HttpClient.{Redirect, Version}
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import scala.util.{Random, Success, Try}
import java.net.{InetSocketAddress, ProxySelector, URI, http => jvm}
import java.nio.channels.{Channels, Pipe}
import java.nio.file.Files
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

  private def request2JVM(request: HttpRequest): IO[jvm.HttpRequest] = {
    var contentType = request.content.map(c => c.contentType.outputString)
    for {
      // TODO: Investigate streaming through InputStream
      bodyPublisher <- request.content match {
        case Some(content: FormDataContent) =>
          val builder = MultipartEntityBuilder.create()
          content.entries.foreach {
            case (key, entry: FileEntry) =>
              val contentType = Headers.`Content-Type`.value(entry.headers).getOrElse(ContentType.`application/octet-stream`)
              val apacheContentType = org.apache.http.entity.ContentType.parse(contentType.outputString)
              builder.addBinaryBody(key, entry.file, apacheContentType, entry.fileName)
            case (key, entry: StringEntry) => builder.addTextBody(key, entry.value)
          }
          val httpEntity = builder.build()
          contentType = Some(httpEntity.getContentType.getValue)

          IO.blocking {
            val baos = new ByteArrayOutputStream
            httpEntity.writeTo(baos)
            val bytes = baos.toByteArray
            BodyPublishers.ofByteArray(bytes)
          }

          // TODO: Figure out why this isn't working - Appears that we're not properly allowing async operations to run
//          val pipe = Pipe.open()
//          for {
//            writingFiber <- IO {
//              val outputStream = Channels.newOutputStream(pipe.sink())
//              httpEntity.writeTo(outputStream)
//            }.start
//            bp <- IO(BodyPublishers.ofInputStream(() => Channels.newInputStream(pipe.source())))
//            _ <- writingFiber.joinWithNever
//          } yield bp
        case Some(content: StringContent) => IO.blocking(BodyPublishers.ofString(content.value))
        case Some(content) => content.asStream.compile.toList.map(_.toArray).map { bytes =>
          BodyPublishers.ofByteArray(bytes)
        }
        case None => IO.pure(BodyPublishers.noBody())
      }
      jvmRequest <- IO.blocking {
        val builder = jvm.HttpRequest.newBuilder()
          .uri(URI.create(request.url.toString.replace("{", "%7B").replace("}", "%7D")))
          .timeout(Duration.ofMillis(client.timeout.toMillis))
          .method(request.method.value, bodyPublisher)
        contentType.foreach { ct =>
          builder.header(Headers.`Content-Type`.key, ct)
        }
        builder.headers(request.headers.map.toList.flatMap {
          case (key, values) => values.flatMap(value => List(key, value))
        }: _*)
        builder.build()
      }
    } yield jvmRequest
  }

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