package spec

import cats.effect.unsafe.implicits.global
import fabric.Json
import fabric.io.{JsonParser, YamlParser}
import spice.streamer._

import scala.collection.mutable

object TestUtils {
  def loadString(name: String): String = Streamer(
    getClass.getClassLoader.getResourceAsStream(name),
    new mutable.StringBuilder
  ).unsafeRunSync().toString

  def loadJson(name: String): Json = JsonParser(loadString(name))

  def loadYaml(name: String): Json = YamlParser(loadString(name))
}