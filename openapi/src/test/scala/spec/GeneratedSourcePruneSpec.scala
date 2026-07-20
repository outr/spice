package spec

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.generator.{GeneratedSourceWriter, SourceFile}

import java.nio.file.{Files, Path}

/**
 * The generated-source writer OWNS its output tree. Pruning only the
 * directories a run emits into leaves a package whose types were ALL
 * removed upstream carrying its stale generated files forever — they
 * surface later as analyzer breakage far from the removal,
 * misattributed to whatever run finally exposed them. The prune walks
 * every emitted directory RECURSIVELY, deletes marker-carrying files
 * that weren't re-emitted, never touches hand-authored files, and
 * removes directories it emptied.
 */
class GeneratedSourcePruneSpec extends AnyWordSpec with Matchers {

  private val Marker = "/// GENERATED CODE: Do not edit!"

  private def seed(root: Path, rel: String, content: String): Path = {
    val p = root.resolve(rel)
    Files.createDirectories(p.getParent)
    Files.writeString(p, content)
    p
  }

  private def emit(path: String, fileName: String): SourceFile =
    SourceFile(language = "Dart", name = fileName.stripSuffix(".dart"), fileName = fileName,
      path = path, source = s"$Marker\nclass Fresh {}\n")

  "GeneratedSourceWriter" should {

    "remove stale generated files from subdirectories with no emissions of their own" in {
      val root = Files.createTempDirectory("prune-spec")
      val stale = seed(root, "lib/model/removed/stale_type.dart", s"$Marker\nclass StaleType {}\n")
      val staleG = seed(root, "lib/model/removed/stale_type.g.dart", s"$Marker\nextension X on Object {}\n")
      GeneratedSourceWriter.write(
        List(emit("lib/model", "current.dart")),
        root, ".dart", Marker
      )
      Files.exists(stale) shouldBe false
      Files.exists(staleG) shouldBe false
      // The emptied subdirectory is gone; the emitted file landed.
      Files.exists(root.resolve("lib/model/removed")) shouldBe false
      Files.exists(root.resolve("lib/model/current.dart")) shouldBe true
    }

    "never touch hand-authored files and keep their directories" in {
      val root = Files.createTempDirectory("prune-spec")
      val hand  = seed(root, "lib/model/custom/hand_made.dart", "class HandMade {}\n")
      val stale = seed(root, "lib/model/custom/old_gen.dart", s"$Marker\nclass OldGen {}\n")
      GeneratedSourceWriter.write(List(emit("lib/model", "current.dart")), root, ".dart", Marker)
      Files.exists(hand) shouldBe true
      Files.exists(stale) shouldBe false
      Files.exists(root.resolve("lib/model/custom")) shouldBe true
    }

    "replace re-emitted files and ignore files with the wrong extension" in {
      val root = Files.createTempDirectory("prune-spec")
      seed(root, "lib/model/current.dart", s"$Marker\nclass Old {}\n")
      val note = seed(root, "lib/model/notes.txt", s"$Marker\nnot dart\n")
      GeneratedSourceWriter.write(List(emit("lib/model", "current.dart")), root, ".dart", Marker)
      Files.readString(root.resolve("lib/model/current.dart")) should include ("class Fresh")
      Files.exists(note) shouldBe true
    }

    "cover nested emitted directories once (outer root owns the walk)" in {
      val root = Files.createTempDirectory("prune-spec")
      val stale = seed(root, "lib/model/sub/removed/gone.dart", s"$Marker\nclass Gone {}\n")
      GeneratedSourceWriter.write(
        List(emit("lib/model", "a.dart"), emit("lib/model/sub", "b.dart")),
        root, ".dart", Marker
      )
      Files.exists(stale) shouldBe false
      Files.exists(root.resolve("lib/model/a.dart")) shouldBe true
      Files.exists(root.resolve("lib/model/sub/b.dart")) shouldBe true
    }
  }
}
