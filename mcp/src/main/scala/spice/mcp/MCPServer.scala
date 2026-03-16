package spice.mcp

import rapid.Task
import spice.http.HttpExchange
import spice.http.server.MutableHttpServer

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

  /**
   * Called during server init to register the MCP HTTP handler.
   */
  protected def initializeMCP(): Unit = {
    handlers += MCPHandler(this)
  }
}
