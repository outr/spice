package spice.net

import scala.annotation.tailrec
import scala.collection.mutable

case class Parameters(entries: List[(String, Param)]) {
  lazy val map: Map[String, Param] = entries.toMap

  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty

  def value(key: String): Option[String] = map.get(key).map(_.value)
  def values(key: String): List[String] = map.get(key).map(_.values).getOrElse(Nil)

  def withParam(key: String, value: String, append: Boolean = true): Parameters = if (value.isEmpty) {
    if (append) {           // Nothing to do
      this
    } else {                // Remove the param if not appending and empty
      removeParam(key)
    }
  } else if (append) {      // Add to the param for this key
    appendParam(key, value)
  } else {                  // Replace the value
    replaceParam(key, List(value))
  }

  def param(key: String, param: Param, append: Boolean = false): Parameters = if (append && value(key).nonEmpty) {
    val updated = Param(values(key) ::: param.values)
    copy(entries = entries.map {
      case (k, _) if key == k => k -> updated
      case p => p
    })
  } else {
    copy(entries = removeParam(key).entries ::: List(key -> param))
  }

  def +(that: Parameters): Parameters = this + that.entries

  @tailrec
  final def +(list: List[(String, Param)]): Parameters = if (list.isEmpty) {
    this
  } else {
    val (key, value) = list.head
    val params = param(key, value)
    params + list.tail
  }

  def appendParam(key: String, value: String): Parameters = {
    val values = this.values(key)
    replaceParam(key, value :: values)
  }

  def replaceParam(key: String, values: List[String]): Parameters = {
    val p = removeParam(key)
    p.copy(entries = (key -> Param(values)) :: p.entries)
  }

  def removeParam(key: String): Parameters = copy(entries = entries.filterNot(p => p._1 == key))

  lazy val encoded: String = {
    if (nonEmpty) {
      val b = new mutable.StringBuilder
      b.append('?')
      val params = map.flatMap {
        case (key, param) =>
          val keyEncoded = Encoder(key)
          if (param.values.nonEmpty) {
            param.values.map { value =>
              val valueEncoded = Encoder(value)
              s"$keyEncoded=$valueEncoded"
            }
          } else {
            List(keyEncoded)
          }
      }.mkString("&")
      b.append(params)
      b.toString
    } else {
      ""
    }
  }

  lazy val decoded: String = {
    if (nonEmpty) {
      val b = new mutable.StringBuilder
      b.append('?')
      val params = map.flatMap {
        case (key, param) =>
          val keyEncoded = key
          if (param.values.nonEmpty) {
            param.values.map { value =>
              val valueEncoded = value
              s"$keyEncoded=$valueEncoded"
            }
          } else {
            List(keyEncoded)
          }
      }.mkString("&")
      b.append(params)
      b.toString
    } else {
      ""
    }
  }

  override def toString: String = encoded
}

object Parameters {
  val empty: Parameters = Parameters(Nil)

  @tailrec
  def parse(query: String): Parameters = if (query.startsWith("?")) {
    parse(query.substring(1))
  } else {
    var params = Parameters.empty
    query.split('&').toList.filter(_.trim.nonEmpty).foreach { s =>
      val equals = s.indexOf('=')
      val (key, values) = if (equals == -1) {
        Decoder(s) -> Nil
      } else {
        Decoder(s.substring(0, equals)) -> List(Decoder(s.substring(equals + 1)))
      }
      params = params.param(key, Param(values), append = true)
    }
    params
  }
}