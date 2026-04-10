package spice.mcp.oauth

import fabric.*
import fabric.io.JsonParser
import rapid.Task
import scribe.mdc.MDC
import spice.http.*
import spice.http.content.Content
import spice.http.server.handler.HttpHandler
import spice.mcp.MCPServer
import spice.net.ContentType

import java.net.{URLDecoder, URLEncoder}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.{Base64, UUID}

case class OAuthHandler(server: MCPServer, config: OAuthConfig, store: OAuthStore) extends HttpHandler {

  private def issuer: String = config.issuer

  override def handle(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    val path = exchange.request.url.path.decoded
    val method = exchange.request.method

    (path, method) match {
      case ("/.well-known/oauth-authorization-server", HttpMethod.Get) => handleMetadata(exchange)
      case ("/oauth/register", HttpMethod.Post) => handleRegister(exchange)
      case ("/oauth/authorize", HttpMethod.Get) => handleAuthorizeForm(exchange)
      case ("/oauth/authorize", HttpMethod.Post) => handleAuthorizeSubmit(exchange)
      case ("/oauth/token", HttpMethod.Post) => handleToken(exchange)
      case _ => Task.pure(exchange) // pass through
    }
  }

  // ── RFC 8414: Authorization Server Metadata ────────────────────────────

  private def handleMetadata(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    val metadata = obj(
      "issuer" -> str(issuer),
      "authorization_endpoint" -> str(s"$issuer/oauth/authorize"),
      "token_endpoint" -> str(s"$issuer/oauth/token"),
      "registration_endpoint" -> str(s"$issuer/oauth/register"),
      "response_types_supported" -> arr(str("code")),
      "grant_types_supported" -> arr(str("authorization_code")),
      "token_endpoint_auth_methods_supported" -> arr(str("none")),
      "code_challenge_methods_supported" -> arr(str("S256"))
    )
    respondJson(exchange, metadata)
  }

  // ── RFC 7591: Dynamic Client Registration ──────────────────────────────

  private def handleRegister(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    readBody(exchange).flatMap {
      case None => respondJsonError(exchange, HttpStatus.BadRequest, "Missing request body")
      case Some(body) =>
        val json = JsonParser(body)
        val clientName = json.get("client_name").map(_.asString).getOrElse("unknown")
        val redirectUris = json.get("redirect_uris")
          .map(_.asVector.map(_.asString).toList)
          .getOrElse(Nil)

        store.registerClient(clientName, redirectUris).flatMap { client =>
          val response = obj(
            "client_id" -> str(client.clientId),
            "client_name" -> str(client.name),
            "redirect_uris" -> arr(client.redirectUris.map(str)*)
          )
          respondJson(exchange, response, HttpStatus.Created)
        }
    }
  }

  // ── Authorization Endpoint (GET = form, POST = submit) ─────────────────

  private def handleAuthorizeForm(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    val params = exchange.request.url.parameters
    val clientId = params.value("client_id").getOrElse("")
    val codeChallenge = params.value("code_challenge").getOrElse("")
    val codeChallengeMethod = params.value("code_challenge_method").getOrElse("S256")
    val redirectUri = params.value("redirect_uri").getOrElse("")
    val state = params.value("state").getOrElse("")
    val error = params.value("error").getOrElse("")

    val errorHtml = if (error.nonEmpty) {
      s"""<div style="background:#ff4444;color:white;padding:10px 16px;border-radius:6px;margin-bottom:16px;font-size:14px;">$error</div>"""
    } else ""

    val html =
      s"""<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="utf-8">
         |  <meta name="viewport" content="width=device-width, initial-scale=1">
         |  <title>Sign In</title>
         |  <style>
         |    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; background: #1a1a2e; color: #eee; display: flex; justify-content: center; align-items: center; min-height: 100vh; margin: 0; }
         |    .card { background: #16213e; padding: 40px; border-radius: 12px; width: 360px; box-shadow: 0 8px 32px rgba(0,0,0,0.3); }
         |    h2 { margin: 0 0 24px; text-align: center; color: #4ecca3; }
         |    label { display: block; font-size: 13px; color: #aaa; margin-bottom: 4px; }
         |    input[type=email], input[type=password] { width: 100%; padding: 10px 12px; border: 1px solid #333; border-radius: 6px; background: #0f3460; color: #eee; font-size: 15px; margin-bottom: 16px; box-sizing: border-box; }
         |    input:focus { outline: none; border-color: #4ecca3; }
         |    button { width: 100%; padding: 12px; background: #4ecca3; color: #1a1a2e; border: none; border-radius: 6px; font-size: 16px; font-weight: 600; cursor: pointer; }
         |    button:hover { background: #3db88c; }
         |  </style>
         |</head>
         |<body>
         |  <div class="card">
         |    <h2>Sign In</h2>
         |    $errorHtml
         |    <form method="POST" action="/oauth/authorize">
         |      <input type="hidden" name="client_id" value="$clientId">
         |      <input type="hidden" name="code_challenge" value="$codeChallenge">
         |      <input type="hidden" name="code_challenge_method" value="$codeChallengeMethod">
         |      <input type="hidden" name="redirect_uri" value="$redirectUri">
         |      <input type="hidden" name="state" value="$state">
         |      <label for="email">Email</label>
         |      <input type="email" id="email" name="email" required autofocus>
         |      <label for="password">Password</label>
         |      <input type="password" id="password" name="password" required>
         |      <button type="submit">Sign In</button>
         |    </form>
         |  </div>
         |</body>
         |</html>""".stripMargin

    exchange.withContent(Content.string(html, ContentType.`text/html`)).map(_.finish())
  }

  private def handleAuthorizeSubmit(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    readFormBody(exchange).flatMap { params =>
      val clientId = params.getOrElse("client_id", "")
      val codeChallenge = params.getOrElse("code_challenge", "")
      val codeChallengeMethod = params.getOrElse("code_challenge_method", "S256")
      val redirectUri = params.getOrElse("redirect_uri", "")
      val state = params.getOrElse("state", "")
      val email = params.getOrElse("email", "")
      val password = params.getOrElse("password", "")

      store.getClient(clientId).flatMap {
        case None => redirectToAuthorize(exchange, params, "Invalid client")
        case Some(_) =>
          server.authenticateUser(email, password).flatMap {
            case None => redirectToAuthorize(exchange, params, "Invalid email or password")
            case Some(ctx) =>
              val code = UUID.randomUUID().toString
              val userId = ctx.store.get("userId").map(_.toString).getOrElse(ctx.sessionId)
              val authCode = AuthorizationCode(
                code = code,
                clientId = clientId,
                userId = userId,
                codeChallenge = codeChallenge,
                codeChallengeMethod = codeChallengeMethod,
                redirectUri = redirectUri,
                state = Some(state).filter(_.nonEmpty),
                expiresAt = System.currentTimeMillis() + config.codeTtl.toMillis,
                context = ctx.store
              )

              store.storeCode(authCode).map { _ =>
                val stateParam = if (state.nonEmpty) s"&state=${enc(state)}" else ""
                val location = s"$redirectUri?code=${enc(code)}$stateParam"
                exchange.copy(response = exchange.response
                  .withStatus(HttpStatus.Found)
                  .withHeader("Location", location)
                ).finish()
              }
          }
      }
    }
  }

  // ── Token Endpoint ─────────────────────────────────────────────────────

  private def handleToken(exchange: HttpExchange)(using MDC): Task[HttpExchange] = {
    readFormBody(exchange).flatMap { params =>
      val grantType = params.getOrElse("grant_type", "")
      val code = params.getOrElse("code", "")
      val codeVerifier = params.getOrElse("code_verifier", "")
      val clientId = params.getOrElse("client_id", "")

      if (grantType != "authorization_code") {
        respondJsonError(exchange, HttpStatus.BadRequest, "unsupported_grant_type")
      } else {
        store.consumeCode(code).flatMap {
          case None =>
            respondJsonError(exchange, HttpStatus.BadRequest, "invalid_grant")
          case Some(authCode) =>
            if (authCode.clientId != clientId) {
              respondJsonError(exchange, HttpStatus.BadRequest, "invalid_grant")
            } else if (!verifyPkce(authCode.codeChallenge, authCode.codeChallengeMethod, codeVerifier)) {
              respondJsonError(exchange, HttpStatus.BadRequest, "invalid_grant")
            } else {
              val tokenValue = UUID.randomUUID().toString
              val expiresIn = config.tokenTtl.toSeconds
              val accessToken = AccessToken(
                token = tokenValue,
                clientId = clientId,
                userId = authCode.userId,
                expiresAt = System.currentTimeMillis() + config.tokenTtl.toMillis,
                context = authCode.context
              )
              store.storeToken(accessToken).flatMap { _ =>
                val response = obj(
                  "access_token" -> str(tokenValue),
                  "token_type" -> str("Bearer"),
                  "expires_in" -> num(expiresIn)
                )
                respondJson(exchange, response)
              }
            }
        }
      }
    }
  }

  // ── PKCE Verification ──────────────────────────────────────────────────

  private def verifyPkce(challenge: String, method: String, verifier: String): Boolean = {
    if (challenge.isEmpty || verifier.isEmpty) return false
    method match {
      case "S256" =>
        val digest = MessageDigest.getInstance("SHA-256").digest(verifier.getBytes(StandardCharsets.US_ASCII))
        val computed = Base64.getUrlEncoder.withoutPadding.encodeToString(digest)
        computed == challenge
      case "plain" => challenge == verifier
      case _ => false
    }
  }

  // ── Helpers ────────────────────────────────────────────────────────────

  private def redirectToAuthorize(exchange: HttpExchange, params: Map[String, String], error: String)(using MDC): Task[HttpExchange] = {
    val qs = params.map { case (k, v) => s"${enc(k)}=${enc(v)}" }.mkString("&")
    val location = s"/oauth/authorize?$qs&error=${enc(error)}"
    Task.pure(exchange.copy(response = exchange.response
      .withStatus(HttpStatus.Found)
      .withHeader("Location", location)
    ).finish())
  }

  private def enc(s: String): String = URLEncoder.encode(s, StandardCharsets.UTF_8)

  private def readBody(exchange: HttpExchange): Task[Option[String]] = {
    exchange.request.content match {
      case Some(content) => content.asString.map(Some(_))
      case None => Task.pure(None)
    }
  }

  private def readFormBody(exchange: HttpExchange): Task[Map[String, String]] = {
    readBody(exchange).map {
      case None => Map.empty
      case Some(body) =>
        body.split("&").flatMap { param =>
          param.split("=", 2) match {
            case Array(k, v) => Some(URLDecoder.decode(k, StandardCharsets.UTF_8) -> URLDecoder.decode(v, StandardCharsets.UTF_8))
            case Array(k) => Some(URLDecoder.decode(k, StandardCharsets.UTF_8) -> "")
            case _ => None
          }
        }.toMap
    }
  }

  private def respondJson(exchange: HttpExchange, json: Json, status: HttpStatus = HttpStatus.OK)(using MDC): Task[HttpExchange] = {
    exchange.modify { response =>
      Task.pure(response
        .withStatus(status)
        .withContent(Content.json(json))
        .withHeader("Cache-Control", "no-store"))
    }.map(_.finish())
  }

  private def respondJsonError(exchange: HttpExchange, status: HttpStatus, error: String)(using MDC): Task[HttpExchange] = {
    respondJson(exchange, obj("error" -> str(error)), status)
  }
}
