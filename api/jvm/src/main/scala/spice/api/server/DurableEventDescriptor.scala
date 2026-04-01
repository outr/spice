package spice.api.server

/** Describes a field on a durable event case class. */
case class DurableFieldDescriptor(name: String, typeName: String, optional: Boolean)

/** Describes one case class variant of a sealed trait used as a wire event. */
case class DurableEventDescriptor(kind: String, fields: List[DurableFieldDescriptor])
