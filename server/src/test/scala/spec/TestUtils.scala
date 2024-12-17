package spec

import fabric.Json
import fabric.io.{JsonParser, YamlParser}
import spice.streamer._

import scala.collection.mutable

object TestUtils {
  def loadString(name: String): String = {
    val stream = getClass.getClassLoader.getResourceAsStream(name)
    if (stream == null) throw new RuntimeException(s"Not found: $name")
    Streamer(
      stream,
      new mutable.StringBuilder
    ).sync().toString
  }

  def loadJson(name: String): Json = JsonParser(loadString(name))

  def loadYaml(name: String): Json = YamlParser(loadString(name))
}