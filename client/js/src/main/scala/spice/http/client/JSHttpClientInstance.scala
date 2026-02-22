package spice.http.client

import moduload.Moduload
import rapid.Task
import spice.ajax.{AjaxAction, AjaxRequest}
import spice.http.content.{Content, StringContent}
import spice.http.{Headers, HttpRequest, HttpResponse, HttpStatus, WebSocket}
import spice.net.{ContentType, URL}

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

class JSHttpClientInstance(client: HttpClient) extends HttpClientInstance {
  assert(client.proxy.isEmpty, "Proxy is not supported in the browser JS client. Proxy configuration is ignored.")

  private val HeaderRegex = """(.+)[:](.+)""".r

  override def send(request: HttpRequest): Task[Try[HttpResponse]] = {
    val manager = client.connectionPool.asInstanceOf[JSConnectionPool].manager
    val contentString = request.content match {
      case Some(content) => content.asString.map(Some.apply)
      case None => Task.pure(None)
    }
    contentString.flatMap { data =>
      val timeoutMs = client.timeout.toMillis.toInt
      val ajaxRequest = new AjaxRequest(
        url = request.url,
        data = data,
        timeout = timeoutMs,
        headers = request.headers.map.flatMap(t => t._2.map(value => t._1 -> value)),
        withCredentials = true,
        responseType = ""
      )
      val action = new AjaxAction(ajaxRequest)
      manager.enqueue(action).map {
        case Failure(err) => Failure(err)
        case Success(xmlHttpRequest) =>
          val headers: Map[String, List[String]] = xmlHttpRequest.getAllResponseHeaders().split('\n').map(_.trim).map {
            case HeaderRegex(key, value) => key.trim -> value.trim
            case s => throw new RuntimeException(s"Invalid Header: [$s]")
          }.groupBy(_._1).map {
            case (key, array) => key -> array.toList.map(_._2)
          }
          val responseContentType = headers.get("Content-Type").flatMap(_.headOption).map(ContentType.parse)
          val content = Option(xmlHttpRequest.responseText).filter(_.nonEmpty).map { text =>
            Content.string(text, responseContentType.getOrElse(ContentType.`text/plain`))
          }
          Success(HttpResponse(
            status = HttpStatus(xmlHttpRequest.status, xmlHttpRequest.statusText),
            headers = Headers(headers),
            content = content
          ))
      }
    }
  }

  override def webSocket(url: URL): WebSocket = new JSWebSocketClient(url)

  override def dispose(): Task[Unit] = Task.unit
}