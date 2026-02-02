package spice.http.server.undertow

import rapid._
import io.undertow.server.HttpServerExchange
import io.undertow.server.handlers.form.{FormDataParser, FormParserFactory}
import io.undertow.util.HeaderMap
import org.xnio.streams.ChannelInputStream
import spice.http.content.{Content, FormDataContent, FormDataEntry, StreamContent}
import spice.http.content.FormDataEntry.{FileEntry, StringEntry}
import spice.http.{Headers, HttpMethod, HttpRequest}
import spice.net.{ContentType, IP, URL}

import java.io.File
import scala.jdk.CollectionConverters.IterableHasAsScala

object UndertowRequestParser {
  private val formParserBuilder = FormParserFactory.builder()

  def apply(exchange: HttpServerExchange, url: URL): Task[HttpRequest] = Task {
    val source = IP
      .fromString(exchange.getSourceAddress.getAddress.getHostAddress)
      .getOrElse {
        scribe.warn(s"Invalid IP address: ${exchange.getSourceAddress.getAddress.getHostAddress}")
        IP.v4(0, 0, 0, 0)
      }
    val headers = parseHeaders(exchange.getRequestHeaders)

    val content: Option[Content] = if (exchange.getRequestContentLength > 0L) {
      Headers.`Content-Type`.value(headers).getOrElse(ContentType.`text/plain`) match {
        case ContentType.`multipart/form-data` =>
          exchange.startBlocking()
          val formDataParser = formParserBuilder.build().createParser(exchange)
          formDataParser.parseBlocking()
          val formData = exchange.getAttachment(FormDataParser.FORM_DATA)
          val data = formData.asScala.toList.map { key =>
            val entries: List[FormDataEntry] = formData.get(key).asScala.map { entry =>
              val headers = parseHeaders(entry.getHeaders)
              if (entry.isFileItem) {
                val path = entry.getFileItem.getFile
                val file = File.createTempFile("spice-form", entry.getFileName)
                path.toFile.renameTo(file)
                FileEntry(entry.getFileName, file, headers)
              } else {
                StringEntry(entry.getValue, headers)
              }
            }.toList
            if (entries.length > 1) throw new UnsupportedOperationException(s"More than one entry for $key found! Not currently supported!")
            key -> entries.head
          }
          Some(FormDataContent(data.toMap))
        case ct =>
          Option(exchange.getRequestChannel) match {
            case Some(channel) =>
              val stream = rapid.Stream.fromInputStream(Task(new ChannelInputStream(channel)))
              Some(StreamContent(stream, ct))
            case None => throw new NullPointerException(s"Channel is null for request channel. Probably already consumed.")
          }
      }
    } else {
      None
    }

    HttpRequest(
      method = HttpMethod(exchange.getRequestMethod.toString),
      source = source,
      url = url,
      headers = headers,
      content = content
    )
  }

  private def parseHeaders(headerMap: HeaderMap): Headers = Headers(headerMap.asScala.map { hv =>
    hv.getHeaderName.toString -> hv.asScala.toList
  }.toMap)
}