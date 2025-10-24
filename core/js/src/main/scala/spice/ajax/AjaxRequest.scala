package spice.ajax

import org.scalajs.dom
import org.scalajs.dom.{ProgressEvent, XMLHttpRequest}
import rapid.Task
import rapid.task.Completable
import reactify._
import spice.UserException
import spice.http.HttpMethod
import spice.net.URL

import scala.scalajs.js
import scala.util.{Failure, Success, Try}

class AjaxRequest(url: URL,
                  method: HttpMethod = HttpMethod.Post,
                  data: Option[String] = None,
                  timeout: Int = 0,
                  headers: Map[String, String] = Map.empty,
                  withCredentials: Boolean = true,
                  responseType: String = "") {
  val req = new dom.XMLHttpRequest()
  val completable: Completable[Try[XMLHttpRequest]] = Task.completable[Try[XMLHttpRequest]]
  val loaded: Val[Double] = Var(0.0)
  val total: Val[Double] = Var(0.0)
  val percentage: Val[Int] = Var(0)
  val cancelled: Val[Boolean] = Var(false)

  req.onreadystatechange = { (_: dom.Event) =>
    if (req.readyState == 4) {
      if ((req.status >= 200 && req.status < 300) || req.status == 304) {
        completable.success(Success(req))
      } else {
        completable.success(Failure(UserException(s"AjaxRequest failed: ${req.readyState}")))
      }
    }
  }
  req.upload.addEventListener("progress", (evt: ProgressEvent) => {
    total.asInstanceOf[Var[Double]] @= evt.total
    loaded.asInstanceOf[Var[Double]] @= evt.loaded
    val p = math.round(math.floor((evt.loaded / evt.total) * 100)).toInt
    percentage.asInstanceOf[Var[Int]] @= p
  })
  req.open(method.value, url.toString)
  req.responseType = responseType
  req.timeout = timeout
  req.withCredentials = withCredentials
  headers.foreach(x => req.setRequestHeader(x._1, x._2))

  def send(): Task[Try[XMLHttpRequest]] = {
    data match {
      case Some(formData) => req.send(formData.asInstanceOf[js.Any])
      case None => req.send()
    }
    completable
  }

  def cancel(): Unit = if (percentage.get != 100) {
    req.abort()
    cancelled.asInstanceOf[Var[Boolean]] @= true
  }
}