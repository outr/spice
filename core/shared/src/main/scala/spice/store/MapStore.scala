package spice.store

class MapStore(var map: Map[String, Any] = Map.empty) extends Store {
  override def get[T](key: String): Option[T] = map.get(key).asInstanceOf[Option[T]]

  override def update[T](key: String, value: T): Unit = synchronized(map += key -> value)

  override def remove(key: String): Unit = synchronized(map -= key)
}
