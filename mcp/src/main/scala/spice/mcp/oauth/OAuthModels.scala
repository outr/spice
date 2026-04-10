package spice.mcp.oauth

case class OAuthClient(
  clientId: String,
  name: String,
  redirectUris: List[String]
)

case class AuthorizationCode(
  code: String,
  clientId: String,
  userId: String,
  codeChallenge: String,
  codeChallengeMethod: String,
  redirectUri: String,
  state: Option[String],
  expiresAt: Long,
  context: Map[String, Any] = Map.empty
)

case class AccessToken(
  token: String,
  clientId: String,
  userId: String,
  expiresAt: Long,
  context: Map[String, Any] = Map.empty
)
