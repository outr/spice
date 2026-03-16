package spice.api.server

object WsIntrospect {
  inline def methods[T]: List[WsMethodDescriptor] = ${ WsIntrospectMacro.introspect[T] }
}
