package spice.http

import fabric.rw._

case class HttpStatus(code: Int, message: String) extends Ordered[HttpStatus] {
  assert(message.trim.nonEmpty)

  HttpStatus.synchronized {
    HttpStatus.codeMap += code -> this
  }

  def isInformation: Boolean = code >= 100 && code < 200
  def isSuccess: Boolean = code >= 200 && code < 300
  def isRedirection: Boolean = code >= 300 && code < 400
  def isClientError: Boolean = code >= 400 && code < 500
  def isServerError: Boolean = code >= 500

  def isError: Boolean = isClientError || isServerError

  def apply(message: String): HttpStatus = copy(message = message)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: HttpStatus => code == that.code
    case _ => false
  }

  override def compare(that: HttpStatus): Int = this.code.compare(that.code)

  override def toString = s"$code: $message"
}

object HttpStatus {
  private var codeMap = Map.empty[Int, HttpStatus]

  implicit val rw: RW[HttpStatus] = RW.gen

  val Continue: HttpStatus = HttpStatus(100, "Continue")
  val SwitchingProtocols: HttpStatus = HttpStatus(101, "Switching Protocols")
  val Processing: HttpStatus = HttpStatus(102, "Processing")

  val OK: HttpStatus = HttpStatus(200, "OK")
  val Created: HttpStatus = HttpStatus(201, "Created")
  val Accepted: HttpStatus = HttpStatus(202, "Accepted")
  val NonAuthoritativeInformation: HttpStatus = HttpStatus(203, "Non-Authoritative Information")
  val NoContent: HttpStatus = HttpStatus(204, "No Content")
  val ResetContent: HttpStatus = HttpStatus(205, "Reset Content")
  val PartialContent: HttpStatus = HttpStatus(206, "Partial Content")
  val MultiStatus: HttpStatus = HttpStatus(207, "Multi-Status")

  val MultipleChoices: HttpStatus = HttpStatus(300, "Multiple Choices")
  val MovedPermanently: HttpStatus = HttpStatus(301, "Moved Permanently")
  val Found: HttpStatus = HttpStatus(302, "Found")
  val SeeOther: HttpStatus = HttpStatus(303, "See Other")
  val NotModified: HttpStatus = HttpStatus(304, "Not Modified")
  val UseProxy: HttpStatus = HttpStatus(305, "Use Proxy")
  val TemporaryRedirect: HttpStatus = HttpStatus(307, "Temporary Redirect")

  val BadRequest: HttpStatus = HttpStatus(400, "Bad Request")
  val Unauthorized: HttpStatus = HttpStatus(401, "Unauthorized")
  val PaymentRequired: HttpStatus = HttpStatus(402, "Payment Required")
  val Forbidden: HttpStatus = HttpStatus(403, "Forbidden")
  val NotFound: HttpStatus = HttpStatus(404, "Not Found")
  val MethodNotAllowed: HttpStatus = HttpStatus(405, "Method Not Allowed")
  val NotAcceptable: HttpStatus = HttpStatus(406, "Not Acceptable")
  val ProxyAuthenticationRequired: HttpStatus = HttpStatus(407, "Proxy Authentication Required")
  val RequestTimeout: HttpStatus = HttpStatus(408, "Request Timeout")
  val Conflict: HttpStatus = HttpStatus(409, "Conflict")
  val Gone: HttpStatus = HttpStatus(410, "Gone")
  val LengthRequired: HttpStatus = HttpStatus(411, "Length Required")
  val PreconditionFailed: HttpStatus = HttpStatus(412, "Precondition Failed")
  val RequestEntityTooLarge: HttpStatus = HttpStatus(413, "Request Entity Too Large")
  val RequestURITooLong: HttpStatus = HttpStatus(414, "Request-URI Too Long")
  val UnsupportedMediaType: HttpStatus = HttpStatus(415, "Unsupported Media Type")
  val RequestedRangeNotSatisfiable: HttpStatus = HttpStatus(416, "Requested Range Not Satisfiable")
  val ExpectationFailed: HttpStatus = HttpStatus(417, "Expectation Failed")
  val UnprocessableEntity: HttpStatus = HttpStatus(422, "Unprocessable Entity")
  val Locked: HttpStatus = HttpStatus(423, "Locked")
  val FailedDependency: HttpStatus = HttpStatus(424, "Failed Dependency")
  val UnorderedCollection: HttpStatus = HttpStatus(425, "Unordered Collection")
  val UpgradeRequired: HttpStatus = HttpStatus(426, "Upgrade Required")
  val PreconditionRequired: HttpStatus = HttpStatus(428, "Precondition Required")
  val TooManyRequests: HttpStatus = HttpStatus(429, "Too Many Requests")
  val RequestHeaderFieldsTooLarge: HttpStatus = HttpStatus(431, "Request Header Fields Too Large")

  val InternalServerError: HttpStatus = HttpStatus(500, "Internal Server Error")
  val NotImplemented: HttpStatus = HttpStatus(501, "Not Implemented")
  val BadGateway: HttpStatus = HttpStatus(502, "Bad Gateway")
  val ServiceUnavailable: HttpStatus = HttpStatus(503, "Service Unavailable")
  val GatewayTimeout: HttpStatus = HttpStatus(504, "Gateway Timeout")
  val HTTPVersionNotSupported: HttpStatus = HttpStatus(505, "HTTP Version Not Supported")
  val VariantAlsoNegotiates: HttpStatus = HttpStatus(506, "Variant Also Negotiates")
  val InsufficientStorage: HttpStatus = HttpStatus(507, "Insufficient Storage")
  val NotExtended: HttpStatus = HttpStatus(510, "Not Extended")
  val NetworkAuthenticationRequired: HttpStatus = HttpStatus(511, "Network Authentication Required")

  def getByCode(code: Int): Option[HttpStatus] = codeMap.get(code)
  def byCode(code: Int): HttpStatus = getByCode(code).getOrElse(throw new RuntimeException(s"Unable to find HttpResponseStatus by code: $code"))
}
