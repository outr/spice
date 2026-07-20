package spec

import fabric.define.{DefType, Definition}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.generator.SourceFile
import spice.openapi.generator.dart.{DurableSocketDartConfig, DurableSocketDartGenerator}

import java.nio.file.Files

/**
 * An unresolved type placeholder in emitted Dart guarantees a broken
 * tree with the error surfacing far from its cause (an analyzer error
 * weeks later in a file nobody hand-edits). The writer refuses to
 * write any source containing `InvalidType` and names the offending
 * files instead.
 */
class DartInvalidTypeGuardSpec extends AnyWordSpec with Matchers {

  private def generator: DurableSocketDartGenerator =
    DurableSocketDartGenerator(DurableSocketDartConfig(
      serviceName = "Test",
      wireType = "Wire" -> Definition(DefType.Poly(Map.empty), className = Some("spec.Wire"))
    ))

  "the Dart writer" should {

    "refuse to write a source file containing InvalidType, naming the file" in {
      val root = Files.createTempDirectory("invalid-type-guard")
      val bad = SourceFile(language = "Dart", name = "broken", fileName = "broken.g.dart",
        path = "lib/model", source = "/// GENERATED CODE: Do not edit!\nvoid f(InvalidType x) {}\n")
      val thrown = intercept[RuntimeException] {
        generator.write(List(bad), root)
      }
      thrown.getMessage should include ("lib/model/broken.g.dart")
      thrown.getMessage should include ("InvalidType")
      Files.exists(root.resolve("lib/model/broken.g.dart")) shouldBe false
    }

    "write clean sources normally" in {
      val root = Files.createTempDirectory("invalid-type-guard")
      val ok = SourceFile(language = "Dart", name = "fine", fileName = "fine.dart",
        path = "lib/model", source = "/// GENERATED CODE: Do not edit!\nclass Fine {}\n")
      generator.write(List(ok), root)
      Files.readString(root.resolve("lib/model/fine.dart")) should include ("class Fine")
    }
  }
}
