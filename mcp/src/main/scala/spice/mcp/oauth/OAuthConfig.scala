package spice.mcp.oauth

import scala.concurrent.duration.*

case class OAuthConfig(
  issuer: String,
  tokenTtl: FiniteDuration = 8.hours,
  codeTtl: FiniteDuration = 10.minutes
)
