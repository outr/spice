package spice.http.server.middleware

import rapid.Task
import scribe.mdc.MDC
import spice.http.{Headers, HttpExchange, HttpStatus}
import spice.http.content.Content
import spice.http.server.dsl.{ConnectionFilter, FilterResponse}

import java.util.Base64

trait Authenticator {
  def authenticate(exchange: HttpExchange): Task[AuthResult]
}

sealed trait AuthResult
object AuthResult {
  case class Authenticated(principal: String) extends AuthResult
  case object Unauthorized extends AuthResult
}

object BasicAuthenticator {
  def apply(realm: String)(validate: (String, String) => Task[Boolean]): Authenticator = new Authenticator {
    override def authenticate(exchange: HttpExchange): Task[AuthResult] = {
      exchange.request.headers.first(Headers.Request.`Authorization`) match {
        case Some(header) if header.startsWith("Basic ") =>
          val decoded = new String(Base64.getDecoder.decode(header.substring(6)), "UTF-8")
          decoded.split(":", 2) match {
            case Array(username, password) =>
              validate(username, password).map {
                case true => AuthResult.Authenticated(username)
                case false => AuthResult.Unauthorized
              }
            case _ => Task.pure(AuthResult.Unauthorized)
          }
        case _ => Task.pure(AuthResult.Unauthorized)
      }
    }
  }
}

object BearerAuthenticator {
  def apply(validate: String => Task[Option[String]]): Authenticator = new Authenticator {
    override def authenticate(exchange: HttpExchange): Task[AuthResult] = {
      exchange.request.headers.first(Headers.Request.`Authorization`) match {
        case Some(header) if header.startsWith("Bearer ") =>
          val token = header.substring(7)
          validate(token).map {
            case Some(principal) => AuthResult.Authenticated(principal)
            case None => AuthResult.Unauthorized
          }
        case _ => Task.pure(AuthResult.Unauthorized)
      }
    }
  }
}

class AuthenticationFilter(authenticator: Authenticator,
                           realm: String = "Restricted",
                           scheme: String = "Bearer") extends ConnectionFilter {
  override def apply(exchange: HttpExchange)(using mdc: MDC): Task[FilterResponse] = {
    authenticator.authenticate(exchange).flatMap {
      case AuthResult.Authenticated(principal) =>
        val updated = exchange.copy(store = exchange.store)
        updated.store(AuthenticationFilter.PrincipalKey) = principal
        Task.pure(continue(updated))
      case AuthResult.Unauthorized =>
        exchange.modify { response =>
          Task.pure(
            response
              .withStatus(HttpStatus.Unauthorized)
              .withHeader(Headers.Response.`WWW-Authenticate`(s"""$scheme realm="$realm""""))
              .withContent(Content.none)
          )
        }.map(stop)
    }
  }
}

object AuthenticationFilter {
  val PrincipalKey: String = "auth.principal"

  def apply(authenticator: Authenticator, realm: String = "Restricted", scheme: String = "Bearer"): AuthenticationFilter =
    new AuthenticationFilter(authenticator, realm, scheme)

  def principal(exchange: HttpExchange): Option[String] =
    exchange.store.get[String](PrincipalKey)
}
