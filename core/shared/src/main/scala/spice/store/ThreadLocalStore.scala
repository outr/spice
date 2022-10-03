package spice.store

class ThreadLocalStore extends Store {
  private val threadLocal = new ThreadLocal[Option[Map[String, Any]]] {
    override def initialValue(): Option[Map[String, Any]] = None
  }

  private def map: Map[String, Any] = threadLocal.get().getOrElse(throw new RuntimeException(s"Not in a thread-local context."))

  private def map_=(map: Map[String, Any]): Unit = threadLocal.set(Some(map))

  override def get[T](key: String): Option[T] = map.get(key).asInstanceOf[Option[T]]

  override def update[T](key: String, value: T): Unit = map = map + (key -> value)

  override def remove(key: String): Unit = map = map - key

  def contextualize[R](f: => R): R = {
    threadLocal.set(Some(Map.empty))
    try {
      f
    } finally {
      threadLocal.remove()
    }
  }
}
