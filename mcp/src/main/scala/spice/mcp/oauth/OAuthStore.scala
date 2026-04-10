package spice.mcp.oauth

import rapid.Task

import java.util.concurrent.ConcurrentHashMap
import java.util.UUID
import scala.jdk.CollectionConverters.*

trait OAuthStore {
  def registerClient(name: String, redirectUris: List[String]): Task[OAuthClient]
  def getClient(clientId: String): Task[Option[OAuthClient]]
  def storeCode(code: AuthorizationCode): Task[Unit]
  def consumeCode(code: String): Task[Option[AuthorizationCode]]
  def storeToken(token: AccessToken): Task[Unit]
  def getToken(token: String): Task[Option[AccessToken]]
  def revokeToken(token: String): Task[Unit]
}

class InMemoryOAuthStore extends OAuthStore {
  private val clients = new ConcurrentHashMap[String, OAuthClient]()
  private val codes = new ConcurrentHashMap[String, AuthorizationCode]()
  private val tokens = new ConcurrentHashMap[String, AccessToken]()

  override def registerClient(name: String, redirectUris: List[String]): Task[OAuthClient] = Task {
    val clientId = UUID.randomUUID().toString
    val client = OAuthClient(clientId = clientId, name = name, redirectUris = redirectUris)
    clients.put(clientId, client)
    client
  }

  override def getClient(clientId: String): Task[Option[OAuthClient]] = Task {
    Option(clients.get(clientId))
  }

  override def storeCode(code: AuthorizationCode): Task[Unit] = Task {
    codes.put(code.code, code)
  }

  override def consumeCode(code: String): Task[Option[AuthorizationCode]] = Task {
    val authCode = Option(codes.remove(code))
    authCode.filter(_.expiresAt > System.currentTimeMillis())
  }

  override def storeToken(token: AccessToken): Task[Unit] = Task {
    tokens.put(token.token, token)
  }

  override def getToken(token: String): Task[Option[AccessToken]] = Task {
    Option(tokens.get(token)).filter(_.expiresAt > System.currentTimeMillis())
  }

  override def revokeToken(token: String): Task[Unit] = Task {
    tokens.remove(token)
  }
}
