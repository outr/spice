package spice.http.client

import org.scalajs.dom.{Blob, BlobPropertyBag, XMLHttpRequest}
import rapid.Task
import spice.UserException
import spice.ajax.{AjaxAction, AjaxRequest}
import spice.http.content.{BytesContent, Content}
import spice.http.{Headers, HttpRequest, HttpResponse, HttpStatus, WebSocket}
import spice.net.{ContentType, URL}

import scala.scalajs.js
import scala.scalajs.js.typedarray.*
import scala.util.{Failure, Success, Try}

class JSHttpClientInstance(client: HttpClient) extends HttpClientInstance {
  assert(client.proxy.isEmpty, "Proxy is not supported in the browser JS client. Proxy configuration is ignored.")

  private val HeaderRegex = """([^:]+):(.*)""".r

  override def send(request: HttpRequest): Task[Try[HttpResponse]] = {
    val manager = client.connectionPool.asInstanceOf[JSConnectionPool].manager
    // bytes go as they are, in a Blob: made into a string they would be decoded as UTF-8, and a binary body mangled
    val body: Task[Option[String | Blob]] = request.content match {
      case Some(content: BytesContent) =>
        Task.pure(Some(new Blob(js.Array(content.value.toTypedArray.buffer), new BlobPropertyBag {})))
      case Some(content) => content.asString.map(Some.apply)
      case None => Task.pure(None)
    }
    body.flatMap { data =>
      val timeoutMs = client.timeout.toMillis.toInt
      // The content's type goes with it, as it does on the JVM, unless a header already names one
      val contentType = request.content match {
        case Some(content) if Headers.`Content-Type`.value(request.headers).isEmpty =>
          Map(Headers.`Content-Type`.key -> content.contentType.outputString)
        case _ => Map.empty[String, String]
      }
      val ajaxRequest = new AjaxRequest(
        url = request.url,
        method = request.method,
        data = data,
        timeout = timeoutMs,
        headers = request.headers.map.flatMap(t => t._2.map(value => t._1 -> value)) ++ contentType,
        withCredentials = client.withCredentials,
        // the response's bytes as they came, which a text response is decoded from and a binary one kept as
        responseType = "arraybuffer"
      )
      val action = new AjaxAction(ajaxRequest)
      // AjaxRequest fails on any status outside 2xx, but a response is a response whatever its status, as on the
      // JVM: only a request that got none (refused, timed out, aborted or blocked by CORS) fails, and is retried
      manager.enqueue(action).map { _ =>
        val xhr = ajaxRequest.req
        if (xhr.status == 0) {
          Failure(UserException(s"No response from ${request.method.value} ${request.url}"))
        } else {
          Success(response(xhr))
        }
      }
    }
  }

  private def response(xhr: XMLHttpRequest): HttpResponse = {
    val headers: Map[String, List[String]] = xhr.getAllResponseHeaders().split('\n').map(_.trim).filter(_.nonEmpty).map {
      case HeaderRegex(key, value) => key.trim -> value.trim
      case s => throw new RuntimeException(s"Invalid Header: [$s]")
    }.groupBy(_._1).map {
      case (key, array) => key -> array.toList.map(_._2)
    }
    val responseContentType = Headers.`Content-Type`.value(Headers(headers))
    val content = Option(xhr.response).filter(r => !js.isUndefined(r)).map(_.asInstanceOf[ArrayBuffer])
      .filter(_.byteLength > 0).map { buffer =>
        responseContentType match {
          // text as the browser would decode it (by its charset, UTF-8 without one), as the other clients give it
          case Some(ct) if textual(ct) => Content.string(decode(buffer, ct.charSet), ct)
          // anything else as its bytes, which a caller may still read as UTF-8 text (asString)
          case other => Content.bytes(new Int8Array(buffer).toArray, other.getOrElse(ContentType.`application/octet-stream`))
        }
      }
    val status = {
      val code = xhr.status
      val text = xhr.statusText
      if (text.trim.nonEmpty) HttpStatus(code, text)
      else HttpStatus.getByCode(code).getOrElse(HttpStatus(code, s"HTTP $code"))
    }
    HttpResponse(
      status = status,
      headers = Headers(headers),
      content = content
    )
  }

  /** Whether a response of this type is words: text, JSON, XML, JavaScript or a form. */
  private def textual(ct: ContentType): Boolean = ct.`type` == "text" || ct.subType == "json" ||
    ct.subType.endsWith("+json") || ct.subType == "xml" || ct.subType.endsWith("+xml") || ct.subType == "javascript" ||
    ct.subType == "x-www-form-urlencoded"

  private def decode(buffer: ArrayBuffer, charSet: Option[String]): String = {
    val decoder = js.Dynamic.global.TextDecoder
    // a charset the browser does not know is read as UTF-8
    val reader = try js.Dynamic.newInstance(decoder)(charSet.getOrElse("utf-8")) catch {
      case _: Throwable => js.Dynamic.newInstance(decoder)("utf-8")
    }
    reader.decode(new Uint8Array(buffer)).asInstanceOf[String]
  }

  override def webSocket(url: URL, headers: Headers): WebSocket = new JSWebSocketClient(url)

  override def dispose(): Task[Unit] = Task.unit
}
