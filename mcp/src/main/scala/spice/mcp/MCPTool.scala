package spice.mcp

import fabric.*
import fabric.rw.*
import rapid.Task

case class MCPToolDef(
  name: String,
  description: String,
  inputSchema: Json
)

object MCPToolDef {
  given rw: RW[MCPToolDef] = RW.gen
}

trait MCPTool {
  def definition: MCPToolDef
  def call(arguments: Json)(using MCPContext): Task[MCPToolResult]
}

case class MCPToolResult(
  content: List[MCPContent],
  isError: Boolean = false
)

object MCPToolResult {
  def text(text: String): MCPToolResult = MCPToolResult(List(MCPTextContent(text)))
  def json(json: Json): MCPToolResult = MCPToolResult(List(MCPJsonContent(json)))
  def error(message: String): MCPToolResult = MCPToolResult(List(MCPTextContent(message)), isError = true)
}
