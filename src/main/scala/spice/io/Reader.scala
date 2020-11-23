package spice.io

trait Reader {
  def length: Option[Long]
  def read(buffer: Array[Byte]): Int
  def close(): Unit
}