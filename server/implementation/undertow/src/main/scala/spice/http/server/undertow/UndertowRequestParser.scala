package spice.http.server.undertow

import cats.effect.IO
import io.undertow.server.HttpServerExchange
import io.undertow.server.handlers.form.FormDataParser
import io.undertow.util.HeaderMap
import org.xnio.streams.ChannelInputStream
import spice.http.content.{Content, FormData, FormDataContent, FormDataEntry, StringContent}
import spice.http.content.FormDataEntry.{FileEntry, StringEntry}
import spice.http.{Headers, HttpMethod, HttpRequest}
import spice.net.{ContentType, IP, URL}
import spice.streamer.Streamer

import scala.collection.mutable
import scala.jdk.CollectionConverters.IterableHasAsScala

object UndertowRequestParser {
  def apply(exchange: HttpServerExchange, url: URL): IO[HttpRequest] = IO {
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
          val formData = exchange.getAttachment(FormDataParser.FORM_DATA)
          val data = formData.asScala.toList.map { key =>
            val entries: List[FormDataEntry] = formData.get(key).asScala.map { entry =>
              val headers = parseHeaders(entry.getHeaders)
              if (entry.isFileItem) {
                val path = entry.getFileItem.getFile
                FileEntry(entry.getFileName, path.toFile, headers)
              } else {
                StringEntry(entry.getValue, headers)
              }
            }.toList
            FormData(key, entries)
          }
          Some(FormDataContent(data))
        case ct =>
          val cis = new ChannelInputStream(exchange.getRequestChannel)
          val data = Streamer(cis, new mutable.StringBuilder).toString
          Some(StringContent(data, ct))
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