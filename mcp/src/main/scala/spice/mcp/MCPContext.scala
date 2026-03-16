package spice.mcp

case class MCPContext(
  sessionId: String,
  store: Map[String, Any] = Map.empty
) {
  def get[T](key: String): Option[T] = store.get(key).map(_.asInstanceOf[T])
  def set(key: String, value: Any): MCPContext = copy(store = store + (key -> value))
}
