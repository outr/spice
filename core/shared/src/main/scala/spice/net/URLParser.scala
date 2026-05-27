package spice.net

import scala.util.Try

object URLParser {

  /** Sigil #296 — known opaque URI schemes. `spice.net.URL` models
    * hierarchical URIs (`scheme://host[:port]/path`); opaque URIs
    * have no host/port/path so the parser fails fast on these
    * schemes rather than silently mangling them
    * (`data:image/png;base64,…` → `https://data:image/png;base64%2C…`
    * was the pre-fix behavior).
    *
    * Allowlist rather than predicate to avoid breaking the
    * `localhost:8080`-style "scheme-less host:port" input the
    * parser has always accepted. */
  val OpaqueSchemes: Set[String] = Set(
    "data", "blob", "javascript", "mailto", "tel", "sms",
    "urn", "magnet", "about"
  )

  def apply(s: String,
            tldValidation: TLDValidation = TLDValidation.Warn,
            defaultProtocol: Protocol = Protocol.Https): Either[URLParseFailure, URL] = {
    val opaqueRejection = detectOpaqueScheme(s)
    if (opaqueRejection.isDefined) return Left(opaqueRejection.get)
    if ((s.contains('.') || s.contains(":")) && !s.startsWith(":") && !s.endsWith(".")) {
      val (protocolOption, stage1) = extractProtocol(s)
      val (hostSection, pathSection) = separateHostAndPath(stage1)
      val (host, port) = separateHostAndPort(hostSection)
      val (stage2, fragment) = extractFragment(pathSection)
      val (stage3, parameters) = extractParameters(stage2)
      val path = URLPath.parse(stage3)

      if (host.contains("..")) {
        Left(URLParseFailure(s"$s has an invalid host", URLParseFailure.InvalidHost))
      } else if (protocolOption.isEmpty && host.contains('@') && !host.contains(':')) {
        Left(URLParseFailure(s"$s appears to be an email address", URLParseFailure.EmailAddress, None))
      } else {
        val protocol = protocolOption.getOrElse(defaultProtocol)
        val url = URL(
          protocol = protocol,
          host = host,
          port = port.orElse(protocol.defaultPort).getOrElse(-1),
          path = path,
          parameters = parameters,
          fragment = fragment
        )

        if (url.ip.isEmpty && url.host.count(_ == ':') > 1) {
          Left(URLParseFailure(s"Invalid host: ${url.host}", URLParseFailure.InvalidHost))
        } else {
          tldValidation match {
            case TLDValidation.Off => Right(url)
            case TLDValidation.Warn | TLDValidation.ExternalOnly =>
              val actualTld = url.hostParts.lastOption
              actualTld match {
                case Some(tld) if url.ip.isEmpty && url.hostParts.length > 1 && !TopLevelDomains.isValid(tld) =>
                  val message = s"Invalid top-level domain: [$tld] for supplied URL: [$s]"
                  tldValidation match {
                    case TLDValidation.Warn =>
                      scribe.warn(message)
                      Right(url)
                    case TLDValidation.ExternalOnly =>
                      Left(URLParseFailure(message, URLParseFailure.InvalidTopLevelDomain))
                    case _ => throw new RuntimeException("Inconceivable!")
                  }
                case _ => Right(url)
              }
          }
        }
      }
    } else {
      Left(URLParseFailure(s"$s is not a valid URL", URLParseFailure.QuickFail))
    }
  }

  def extractProtocol(s: String): (Option[Protocol], String) = if (s.contains("://")) {
    val index = s.indexOf("://")
    val content = s.substring(0, index)
    val protocol = Protocol(content)
    (Some(protocol), s.substring(index + 3))
  } else if (s.startsWith("//")) {
    (Some(Protocol.Https), s.substring(2))
  } else {
    (None, s)
  }

  def separateHostAndPath(s: String): (String, String) = if (s.contains('/')) {
    val index = s.indexOf('/')
    (s.substring(0, index), s.substring(index))
  } else if (s.contains('?')) {
    val index = s.indexOf('?')
    (s.substring(0, index), s.substring(index))
  } else {
    (s, "")
  }

  def separateHostAndPort(s: String): (String, Option[Int]) = if (s.contains(':')) {
    val index = s.lastIndexOf(':')
    val host = s.substring(0, index)
    val port = Try(s.substring(index + 1).toInt).toOption
    if (port.isEmpty) {
      (s, None)
    } else {
      (host, port)
    }
  } else {
    (s, None)
  }

  def extractFragment(s: String): (String, Option[String]) = if (s.contains('#')) {
    val index = s.indexOf('#')
    (s.substring(0, index), Some(s.substring(index + 1)))
  } else {
    (s, None)
  }

  def extractParameters(s: String): (String, Parameters) = if (s.contains('?')) {
    val index = s.indexOf('?')
    val pre = s.substring(0, index)
    val post = s.substring(index + 1)
    val params = Parameters.parse(post)
    (pre, params)
  } else {
    (s, Parameters.empty)
  }

  /** Sigil #296 — detect a known opaque URI scheme on the input
    * (`data:`, `blob:`, `mailto:`, etc.). Returns `Some(failure)` when
    * the input starts with one of [[OpaqueSchemes]] followed by a
    * colon AND NOT followed by `//`. `None` for hierarchical URLs and
    * `host:port`-style inputs the parser has always accepted. */
  private def detectOpaqueScheme(s: String): Option[URLParseFailure] = {
    val colonIdx = s.indexOf(':')
    if (colonIdx <= 0) return None
    if (s.length > colonIdx + 2 && s.charAt(colonIdx + 1) == '/' && s.charAt(colonIdx + 2) == '/') return None
    val scheme = s.substring(0, colonIdx).toLowerCase
    if (!OpaqueSchemes.contains(scheme)) return None
    Some(URLParseFailure(
      s"$s carries an opaque URI scheme (`$scheme:`); spice.net.URL models hierarchical URIs only.",
      URLParseFailure.OpaqueScheme
    ))
  }
}