package spice

import spice.delta.types.Delta
import spice.http.HttpExchange

package object delta {
  class DeltasOps(exchange: HttpExchange) {
    private val key: String = "deltas"
    def apply(): List[Delta] = exchange.store.getOrElse[List[Delta]](key, Nil)
    def clear(): Unit = exchange.store.remove(key)
    def ++=(deltas: List[Delta]): Unit = exchange.store(key) = apply() ::: deltas
    def +=(delta: Delta): Unit = this ++= List(delta)
    def isEmpty: Boolean = apply().isEmpty
    def nonEmpty: Boolean = apply().nonEmpty
  }

  extension (exchange: HttpExchange)
    def deltas: DeltasOps = new DeltasOps(exchange)
}
