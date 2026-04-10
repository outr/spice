package spice.mcp

import rapid.Task
import spice.http.HttpExchange
import spice.http.server.MutableHttpServer
import spice.mcp.oauth.{InMemoryOAuthStore, OAuthConfig, OAuthHandler, OAuthStore}

trait MCPServer { self: MutableHttpServer =>
  def mcpName: String
  def mcpVersion: String
  def mcpPath: String = "/mcp"

  def tools: List[MCPTool] = Nil
  def resources: List[MCPResource] = Nil

  /**
   * Auth hook — override to validate API keys, tokens, etc.
   * Return None to reject (401), Some(context) to proceed.
   * Default: no auth required (allows all requests).
   */
  def authenticateMCP(exchange: HttpExchange): Task[Option[MCPContext]] =
    Task.pure(Some(MCPContext(sessionId = "")))

  /** OAuth configuration. Override to enable OAuth login for MCP clients like Claude Desktop. */
  def oauthConfig: Option[OAuthConfig] = None

  /** OAuth storage for clients, codes, and tokens. Override for persistent storage. */
  lazy val oauthStore: OAuthStore = new InMemoryOAuthStore

  /**
   * Validate user credentials (email/password) for OAuth login.
   * Override to integrate with your user authentication system.
   * Return Some(MCPContext) on success with userId/orgId in the store map.
   */
  def authenticateUser(email: String, password: String): Task[Option[MCPContext]] =
    Task.pure(None)

  /**
   * Called during server init to register the MCP HTTP handler and optional OAuth handler.
   */
  protected def initializeMCP(): Unit = {
    oauthConfig.foreach { config =>
      handlers += OAuthHandler(this, config, oauthStore)
    }
    handlers += MCPHandler(this)
  }
}
