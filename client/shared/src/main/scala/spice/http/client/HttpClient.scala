package spice.http.client

import fabric.Json
import fabric.io.{Format, JsonFormatter, JsonParser}
import fabric.rw._
import rapid.Task
import spice.http._
import spice.http.client.intercept.Interceptor
import spice.http.content.{Content, StringContent}
import spice.http.cookie.Cookie
import spice.net.{ContentType, DNS, URL, URLPath}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.{Failure, Success, Try}

case class HttpClient(request: HttpRequest,
                      implementation: HttpClientImplementation,
                      retryManager: RetryManager,
                      interceptor: Interceptor,
                      saveDirectory: String,
                      timeout: FiniteDuration,
                      pingInterval: Option[FiniteDuration],
                      dns: DNS,
                      dropNullValuesInJson: Boolean,
                      sessionManager: Option[SessionManager],
                      failOnHttpStatus: Boolean,
                      validateSSLCertificates: Boolean,
                      proxy: Option[Proxy],
                      failures: Int) {
  private[client] lazy val instanceKey: String = List(
    implementation.getClass.getName,
    timeout,
    pingInterval,
    dns,
    validateSSLCertificates,
    proxy
  ).map(_.toString).mkString(",")
  private lazy val instance: HttpClientInstance = implementation.instance(this)

  def connectionPool: ConnectionPool = ConnectionPool(this)

  def modify(f: HttpRequest => HttpRequest): HttpClient = copy(request = f(request))

  def url: URL = request.url
  def url(url: URL): HttpClient = modify(_.copy(url = url))
  def path: URLPath = url.path
  def path(path: URLPath, append: Boolean = false): HttpClient = if (append) {
    modify(_.copy(url = request.url.withPath(request.url.path.merge(path))))
  } else {
    modify(_.copy(url = request.url.withPath(path)))
  }
  def params(params: (String, String)*): HttpClient = modify(_.copy(url = request.url.withParams(params.toMap)))
  def param[T](name: String, value: T, default: T): HttpClient = if (value != default) {
    value match {
      case s: String => params(name -> s)
      case b: Boolean => params(name -> b.toString)
      case i: Int => params(name -> i.toString)
      case l: Long => params(name -> l.toString)
      case l: List[Any] => params(name -> l.mkString(","))
      case s: Some[Any] => param[Any](name, s.head, default)
      case None => this
      case _ => throw new RuntimeException(s"Unsupported param type: $value (${value.getClass.getSimpleName})")
    }
  } else {
    this
  }
  def appendParams(params: (String, String)*): HttpClient = modify(_.copy(url = request.url.withParams(params.toMap, append = true)))

  def method: HttpMethod = request.method
  def method(method: HttpMethod): HttpClient = modify(_.copy(method = method))
  def get: HttpClient = method(HttpMethod.Get)
  def post: HttpClient = method(HttpMethod.Post)
  def header(header: Header): HttpClient = this.header(header, replace = true)
  def header(header: Header, replace: Boolean): HttpClient = {
    if (replace) {
      modify(r => r.copy(headers = r.headers.setHeader(header)))
    } else {
      modify(r => r.copy(headers = r.headers.withHeader(header)))
    }
  }
  def header(key: String, value: String): HttpClient = header(key, value, replace = true)
  def header(key: String, value: String, replace: Boolean): HttpClient = header(Header(HeaderKey(key), value), replace)
  def headers(headers: Headers, replace: Boolean = false): HttpClient = if (replace) {
    modify(_.copy(headers = headers))
  } else {
    modify(_.copy(headers = request.headers.merge(headers)))
  }
  def removeHeader(key: String): HttpClient = modify(r => r.copy(headers = r.headers.removeHeader(HeaderKey(key))))

  def retryManager(manager: RetryManager): HttpClient = copy(retryManager = manager)
  def interceptor(interceptor: Interceptor): HttpClient = copy(interceptor = interceptor)
  def saveDirectory(saveDirectory: String): HttpClient = copy(saveDirectory = saveDirectory)
  def timeout(timeout: FiniteDuration): HttpClient = copy(timeout = timeout)
  def pingInterval(pingInterval: Option[FiniteDuration]): HttpClient = copy(pingInterval = pingInterval)
  def dns(dns: DNS): HttpClient = copy(dns = dns)
  def sessionManager(sessionManager: SessionManager): HttpClient = copy(sessionManager = Some(sessionManager))
  def clearSessionManager(): HttpClient = copy(sessionManager = None)
  def session(session: Session): HttpClient = copy(sessionManager = Some(new SessionManager(session)))
  def dropNullValuesInJson(dropNullValuesInJson: Boolean): HttpClient = copy(dropNullValuesInJson = dropNullValuesInJson)
  def failOnHttpStatus(failOnHttpStatus: Boolean): HttpClient = copy(failOnHttpStatus = failOnHttpStatus)
  def noFailOnHttpStatus: HttpClient = failOnHttpStatus(failOnHttpStatus = false)
  def ignoreSSLCertificates: HttpClient = copy(validateSSLCertificates = false)
  def proxy(proxy: Proxy): HttpClient = copy(proxy = Some(proxy))

  /**
   * Sets the content to be sent. If this request is set to GET, it will automatically be changed to POST.
   *
   * @param content the content to set
   * @return HttpClient
   */
  def content(content: Content): HttpClient = modify(r => r.copy(
    content = Some(content),
    method = if (r.method == HttpMethod.Get) HttpMethod.Post else r.method)
  )

  /**
   * Sets the content to be sent optionally. If this request is set to GET, it will automatically be changed to POST.
   *
   * @param content the content to set - if None, nothing will be changed
   * @return HttpClient
   */
  def content(content: Option[Content]): HttpClient = content match {
    case Some(c) => this.content(c)
    case None => this
  }

  /**
   * Convenience method to sending JSON content.
   *
   * @param json the JSON content to send
   * @return HttpClient
   */
  def json(json: Json): HttpClient = content(StringContent(JsonFormatter.Default(json), ContentType.`application/json`))

  /**
   * Sends an HttpRequest and receives an asynchronous HttpResponse future.
   *
   * @return Future[HttpResponse]
   */
  final def sendTry(): Task[Try[HttpResponse]] = {
    val updatedHeaders = sessionManager match {
      case Some(sm) =>
        val cookieHeaders = sm.session.cookies.map { cookie =>
          Cookie.Request(name = cookie.name, value = cookie.value).http
        } ::: Headers.Request.`Cookie`.value(request.headers).map(_.http).distinct
        request.headers.withHeaders(Headers.Request.`Cookie`.key, cookieHeaders)
      case None => request.headers
    }
    val io = for {
      updatedRequest <- interceptor.before(request.copy(headers = updatedHeaders))
      responseTry <- instance.send(updatedRequest)
      updatedResponse <- interceptor.after(updatedRequest, responseTry)
    } yield {
      updatedResponse
    }
    io.flatMap {
      case Success(response) =>
        sessionManager.foreach { sm =>
          val cookies = response.cookies
          sm(cookies)
        }

        Task.pure(Success(response))
      case Failure(t) =>
        val failures = this.failures + 1
        val client = copy(failures = failures)
        val io = client.sendTry()
        retryManager.retry(request, io, failures, t)
    }
  }

  final def send(): Task[HttpResponse] = sendTry().map {
    case Success(response) => response
    case Failure(exception) => throw exception
  }

  /**
   * Builds on the send method by supporting basic restful calls that calls a URL and returns a case class as the
   * response.
   *
   * @tparam Response the response type
   * @return Try[Response]
   */
  def callTry[Response: RW]: Task[Try[Response]] = sendTry().flatMap {
    case Success(response) =>
      val contentString = response.content match {
        case Some(content) => content.asString
        case None => Task.pure("")
      }
      contentString.map { responseJson =>
        if (!failOnHttpStatus || response.status.isSuccess) {
          if (responseJson.isEmpty) throw new ClientException(s"No content received in response for ${request.url}.", request, response, None)
          Success(JsonParser(responseJson, Format.Json).as[Response])
        } else {
          throw new ClientException(s"HttpStatus was not successful for ${request.url}: ${response.status} - ${response.content.map(_.asString)}", request, response, None)
        }
      }
    case Failure(exception) => throw exception
  }

  /**
   * Builds on the send method by supporting basic restful calls that calls a URL and returns a case class as the
   * response.
   *
   * @tparam Response the response type
   * @return Response
   */
  def call[Response: RW]: Task[Response] = callTry[Response].map {
    case Success(response) => response
    case Failure(throwable) => throw throwable
  }

  /**
   * Builds on the send method by supporting basic restful calls that take a case class as the request and returns a
   * case class as the response.
   *
   * @param request the request object to convert to JSON and send
   * @tparam Request the request type
   * @tparam Response the response type
   * @return Future[Response]
   */
  def restfulTry[Request: RW, Response: RW](request: Request): Task[Try[Response]] = {
    val requestJson = request.json
    method(if (method == HttpMethod.Get) HttpMethod.Post else method).json(requestJson).callTry[Response]
  }

  def restful[Request: RW, Response: RW](request: Request): Task[Response] = restfulTry[Request, Response](request).map {
    case Success(response) => response
    case Failure(throwable) => throw throwable
  }

  /**
   * Similar to the restful call, but provides a different return-type if the response is an error.
   *
   * @param request the request object to convert to JSON and send
   * @tparam Request the request type
   * @tparam Success the success (OK response) response type
   * @tparam Failure the failure (non-OK response) response type
   * @return either Failure or Success
   */
  def restfulEither[Request: RW, Success: RW, Failure: RW](request: Request): Task[Either[Failure, Success]] = {
    val requestJson = request.json
    method(if (method == HttpMethod.Get) HttpMethod.Post else method).json(requestJson).send().flatMap { response =>
      val contentString = response.content match {
        case Some(content) => content.asString
        case None => Task.pure("")
      }
      contentString.map { responseJson =>
        if (responseJson.isEmpty) throw new ClientException(s"No content received in response for ${this.request.url}.", this.request, response, None)
        if (response.status.isSuccess) {
          Right(JsonParser(responseJson, Format.Json).as[Success])
        } else {
          Left(JsonParser(responseJson, Format.Json).as[Failure])
        }
      }
    }
  }

  def webSocket(): WebSocket = instance.webSocket(request.url)

  def dispose(): Task[Unit] = implementation.dispose()
}

object HttpClient extends HttpClient(
  request = HttpRequest(),
  implementation = HttpClientImplementationManager(()),
  retryManager = RetryManager.simple(
    retries = 2,
    delay = 1.second
  ),
  interceptor = Interceptor.empty,
  saveDirectory = ClientPlatform.defaultSaveDirectory,
  timeout = 60.seconds,
  pingInterval = None,
  dns = DNS.default,
  dropNullValuesInJson = false,
  sessionManager = None,
  failOnHttpStatus = true,
  validateSSLCertificates = true,
  proxy = None,
  failures = 0
)