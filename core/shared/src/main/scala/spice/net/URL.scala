package spice.net

import fabric.define.DefType

import scala.util.matching.Regex
import fabric.rw._

import scala.collection.mutable

case class URL(protocol: Protocol = Protocol.Http,
               host: String = "localhost",
               port: Int = 80,
               path: URLPath = URLPath.empty,
               parameters: Parameters = Parameters.empty,
               fragment: Option[String] = None) {
  lazy val hostParts: Vector[String] = host.split('.').toVector
  lazy val ip: Option[IP] = IP.fromString(host)
  lazy val tld: Option[String] = if (hostParts.length > 1 && ip.isEmpty) {
    Some(hostParts.last)
  } else {
    None
  }
  // TODO: Update domain to properly represent hostname when tld is longer than one part
  lazy val domain: String = if (ip.nonEmpty) {
    host
  } else {
    hostParts.takeRight(2).mkString(".")
  }

  def replaceBase(base: String): URL = URL.parse(s"$base${encoded.pathAndArgs}")
  def replacePathAndParams(pathAndParams: String): URL = URL.parse(s"$base$pathAndParams")

  def withProtocol(protocol: Protocol): URL = copy(protocol = protocol)
  def withPort(port: Int): URL = copy(port = port)

  def withPart(part: String): URL = if (part.indexOf("://") != -1) {
    URL.parse(part)
  } else if (part.startsWith("//")) {
    URL.parse(s"${protocol.scheme}:$part")
  } else if (part.startsWith("?")) {
    copy(parameters = Parameters.parse(part))
  } else {
    val index = part.indexOf('?')
    if (index == -1) {
      withPath(part).copy(parameters = Parameters.empty)
    } else {
      val path = part.substring(0, index)
      val params = part.substring(index + 1)
      withPath(path).copy(parameters = parameters + Parameters.parse(params))
    }
  }

  def withPath(path: String, absolutize: Boolean = true): URL = {
    val updated = this.path.append(path).absolute
    copy(path = updated)
  }

  def withPath(path: URLPath): URL = copy(path = path)

  def withFragment(fragment: String): URL = copy(fragment = Option(fragment))
  def withoutFragment(): URL = copy(fragment = None)

  def withParam(key: String, value: String, append: Boolean = true): URL = {
    copy(parameters = parameters.withParam(key, value, append))
  }
  def withParams(params: Map[String, String], append: Boolean = false): URL = {
    var u = this
    params.foreach {
      case (key, value) => u = u.withParam(key, value, append)
    }
    u
  }
  def appendParam(key: String, value: String): URL = copy(parameters = parameters.appendParam(key, value))
  def replaceParam(key: String, values: List[String]): URL = copy(parameters = parameters.replaceParam(key, values))
  def removeParam(key: String): URL = copy(parameters = parameters.removeParam(key))

  def paramList(key: String): List[String] = parameters.values(key)
  def param(key: String): Option[String] = paramList(key).headOption
  def clearParams(): URL = copy(parameters = Parameters.empty)

  lazy val base: String = {
    val b = new mutable.StringBuilder
    b.append(protocol.scheme)
    b.append("://")
    b.append(host)
    if (!protocol.defaultPort.contains(port) && port != -1) {
      b.append(s":$port")       // Not using the default port for the protocol
    }
    b.toString()
  }

  lazy val encoded: URLParts = new URLParts(encoded = true)
  lazy val decoded: URLParts = new URLParts(encoded = false)

  /**
   * Encodes this URL as a complete path. This is primarily useful for caching to a file while avoiding duplicates with
   * the same file name. For example:
   *
   * http://www.example.com/some/path/file.txt
   *
   * Would be encoded to:
   *
   * /www.example.com/some/path/file.txt
   *
   * @param includePort whether the port should be included as a part of the path. Defaults to false.
   */
  def asPath(includePort: Boolean = false): String = if (includePort) {
    s"/$host/$port${path.encoded}"
  } else {
    s"/$host${path.encoded}"
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case url: URL => url.toString == toString
    case _ => false
  }

  override def toString: String = encoded.asString

  class URLParts(encoded: Boolean) {
    def base: String = URL.this.base
    lazy val pathAndArgs: String = {
      val b = new mutable.StringBuilder
      b.append(path)
      b.append(if (encoded) parameters.encoded else parameters.decoded)
      fragment.foreach { f =>
        b.append('#')
        b.append(f)
      }
      b.toString()
    }
    lazy val asString: String = s"$base$pathAndArgs"

    override def toString: String = asString
  }
}

object URL {
  implicit val rw: RW[URL] = RW.from(_.toString.json, v => parse(v.asStr.value), DefType.Str)

  def build(protocol: String,
            host: String,
            port: Int,
            path: String,
            parameters: List[(String, List[String])],
            fragment: Option[String]): URL = {
    val params = Parameters(parameters.map(t => t._1 -> Param(t._2)))
    URL(Protocol(protocol), host, port, URLPath.parse(path), params, fragment)
  }

  def parse(url: String,
            validateTLD: Boolean = true,
            defaultProtocol: Protocol = Protocol.Https): URL = get(url, validateTLD, defaultProtocol) match {
    case Left(parseFailure) => throw MalformedURLException(s"Unable to parse URL: [$url] (${parseFailure.message})", url, parseFailure.cause)
    case Right(url) => url
  }

  def get(url: String,
          validateTLD: Boolean = true,
          defaultProtocol: Protocol = Protocol.Https): Either[URLParseFailure, URL] = URLParser(
    s = url,
    validateTLD = validateTLD,
    defaultProtocol = defaultProtocol
  )

  private val unreservedCharacters = Set('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '-', '_', '.', '~', '+', '='
  )

  private val encodedRegex = """%([a-zA-Z0-9]{2})""".r

  def encode(part: String): String = part.map {
    case c if unreservedCharacters.contains(c) => c
    case c => s"%${c.toLong.toHexString.toUpperCase}"
  }.mkString

  def decode(part: String): String = try {
    encodedRegex.replaceAllIn(part.replace("\\", "\\\\"), (m: Regex.Match) => {
      val g = m.group(1)
      val code = Integer.parseInt(g, 16)
      val c = code.toChar
      if (c == '\\') {
        "\\\\"
      } else {
        c.toString
      }
    })
  } catch {
    case t: Throwable => throw new RuntimeException(s"Failed to decode: [$part]", t)
  }

  def unapply(url: String): Option[URL] = get(url).toOption
}