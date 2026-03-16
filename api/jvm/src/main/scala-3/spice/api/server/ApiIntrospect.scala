package spice.api.server

object ApiIntrospect {
  inline def methods[T]: List[ApiMethodDescriptor] = ${ ApiIntrospectMacro.introspect[T] }
}
