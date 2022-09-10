package spice.stream

trait Writer {
  def write(buffer: Array[Byte], offset: Int, length: Int): Unit
  def flush(): Unit
  def complete(): Unit
  def close(): Unit
}