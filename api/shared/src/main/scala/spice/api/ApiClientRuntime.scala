package spice.api

import fabric.{Json, obj}
import fabric.rw.*
import rapid.Task
import spice.http.client.HttpClient
import spice.net.{URL, URLPath}

object ApiClientRuntime {
  def doGet[R: RW](baseUrl: URL, methodName: String): Task[R] =
    HttpClient
      .url(baseUrl.withPath(baseUrl.path.merge(URLPath.parse("/" + methodName))))
      .get
      .call[R]

  def doRestful[Req: RW, Res: RW](baseUrl: URL, methodName: String, request: Req): Task[Res] =
    HttpClient
      .url(baseUrl.withPath(baseUrl.path.merge(URLPath.parse("/" + methodName))))
      .restful[Req, Res](request)

  def doJson[R: RW](baseUrl: URL, methodName: String, params: List[(String, Json)]): Task[R] =
    HttpClient
      .url(baseUrl.withPath(baseUrl.path.merge(URLPath.parse("/" + methodName))))
      .post
      .json(obj(params*))
      .call[R]
}
