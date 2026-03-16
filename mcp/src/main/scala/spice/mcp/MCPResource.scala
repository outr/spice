package spice.mcp

import fabric.*
import fabric.rw.*
import rapid.Task

case class MCPResourceDef(
  uri: String,
  name: String,
  description: String,
  mimeType: Option[String] = None
)

object MCPResourceDef {
  given rw: RW[MCPResourceDef] = RW.gen
}

trait MCPResource {
  def definition: MCPResourceDef
  def read(uri: String)(using MCPContext): Task[MCPResourceContent]
}

case class MCPResourceContent(
  uri: String,
  mimeType: Option[String] = None,
  text: Option[String] = None
)

object MCPResourceContent {
  given rw: RW[MCPResourceContent] = RW.gen
}
