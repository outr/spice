package spice.api.server

object DurableEventIntrospect {
  /** Introspect a sealed trait to extract its case class variants as event descriptors.
    * Each case class becomes a DurableEventDescriptor with its simple name (converted to snake_case)
    * and fields extracted from the constructor parameters. */
  inline def events[T]: List[DurableEventDescriptor] = ${ DurableEventIntrospectMacro.introspect[T] }
}
