package spice.http.client

import org.scalajs.dom.XMLHttpRequest
import rapid.Task
import spice.UserException
import spice.ajax.{AjaxAction, AjaxRequest}
import spice.http.content.Content
import spice.http.{Headers, HttpRequest, HttpResponse, HttpStatus, WebSocket}
import spice.net.{ContentType, URL}

import scala.util.{Failure, Success, Try}

class JSHttpClientInstance(client: HttpClient) extends HttpClientInstance {
  assert(client.proxy.isEmpty, "Proxy is not supported in the browser JS client. Proxy configuration is ignored.")

  private val HeaderRegex = """([^:]+):(.*)""".r

  override def send(request: HttpRequest): Task[Try[HttpResponse]] = {
    val manager = client.connectionPool.asInstanceOf[JSConnectionPool].manager
    val contentString = request.content match {
      case Some(content) => content.asString.map(Some.apply)
      case None => Task.pure(None)
    }
    contentString.flatMap { data =>
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
        responseType = ""
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
    val content = Option(xhr.responseText).filter(_.nonEmpty).map { text =>
      Content.string(text, responseContentType.getOrElse(ContentType.`text/plain`))
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

  override def webSocket(url: URL, headers: Headers): WebSocket = new JSWebSocketClient(url)

  override def dispose(): Task[Unit] = Task.unit
}
