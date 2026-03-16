package spice.mcp

import fabric.*
import fabric.rw.*

sealed trait MCPContent {
  def toJson: Json
}

case class MCPTextContent(text: String) extends MCPContent {
  override def toJson: Json = obj("type" -> str("text"), "text" -> str(text))
}

case class MCPJsonContent(json: Json) extends MCPContent {
  override def toJson: Json = obj("type" -> str("text"), "text" -> str(json.toString))
}
