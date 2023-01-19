package spice.http.client

import cats.effect.IO
import moduload.Moduload
import spice.ajax.{AjaxAction, AjaxRequest}
import spice.http.content.{Content, StringContent}
import spice.http.{Headers, HttpRequest, HttpResponse, HttpStatus}
import spice.net.ContentType

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

class JSHttpClientInstance(client: HttpClient) extends HttpClientInstance {
  assert(client.proxy.isEmpty, "Proxy is not supported in JSHttpClientImplementation")

  private val HeaderRegex = """(.+)[:](.+)""".r

  override def send(request: HttpRequest): IO[Try[HttpResponse]] = {
    val manager = client.connectionPool.asInstanceOf[JSConnectionPool].manager
    val ajaxRequest = new AjaxRequest(
      url = request.url,
      data = request.content.map(JSHttpClientImplementation.content2String),
      timeout = 0,
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
        val content = xmlHttpRequest.responseType match {
          case null => None
          case _ => {
            val `type` = if (xmlHttpRequest.responseType == "") ContentType.`text/plain` else ContentType.parse(xmlHttpRequest.responseType)
            Some(Content.string(xmlHttpRequest.responseText, `type`))
          }
        }
        Success(HttpResponse(
          status = HttpStatus(xmlHttpRequest.status, xmlHttpRequest.statusText),
          headers = Headers(headers),
          content = content
        ))
    }
  }

  override def dispose(): IO[Unit] = IO.unit
}