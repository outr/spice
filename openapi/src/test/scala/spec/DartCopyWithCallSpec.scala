package spec

import fabric.rw.*
import org.scalatest.funsuite.AnyFunSuite
import spice.openapi.generator.dart.{DurableSocketDartConfig, DurableSocketDartGenerator}

class DartCopyWithCallSpec extends AnyFunSuite {
  case class WithCall(gist: String, call: Option[String]) derives RW
  case class Ordinary(gist: String) derives RW
  private def source[A: RW](name: String): String = DurableSocketDartGenerator(
    DurableSocketDartConfig(serviceName = "Test", wireType = name -> summon[RW[A]].definition))
    .generate().find(_.fileName == (if (name == "WithCall") "with_call.dart" else "ordinary.dart")).get.source

  test("a call field retains its wire name and constructor with a compilable copyWith proxy") {
    val dart = source[WithCall]("WithCall")
    assert(dart.contains("@CopyWith(skipFields: true)"))
    assert(dart.contains("final String? call;"))
    assert(dart.contains("json['call']"))
  }
  test("ordinary models retain individual copyWith setters") {
    assert(source[Ordinary]("Ordinary").contains("@CopyWith()"))
  }
}
